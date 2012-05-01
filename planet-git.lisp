;; Planet-Git a source code repository manager.
;; Copyright (C) 2011-2012 Russell Sim <russell.sim@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; planet-git.lisp

(in-package #:planet-git)


(require :swank)
(require :hunchentoot)
(require :cl-who)
(require :postmodern)
(require :cl-ppcre)


;;; Global Config

(defparameter *repository-directory* #p"/tmp/")

;;; Webserver

(defun resource-path (path)
  "looks up path relative to whereever this asdf system is installed.  Returns a truename"
  (truename (asdf:system-relative-pathname :planet-git path)))


(setq hunchentoot:*dispatch-table*
 (list
  'hunchentoot:dispatch-easy-handlers
  'dispatch-rest-handlers
  (hunchentoot:create-regex-dispatcher "^/?$" 'home-page)
  (hunchentoot:create-regex-dispatcher "^/[^/]+/$" 'user-page)
  (hunchentoot:create-regex-dispatcher "^/[^/]+/[^/]+/$" 'repository-home-page)
  (hunchentoot:create-regex-dispatcher "^/[^/]+/[^/]+/branch/[^/]+/$" 'repository-branch-page)
  (hunchentoot:create-regex-dispatcher "^/[^/]+/[^/]+/key/[^/]+/$" 'repository-key-page)
  (hunchentoot:create-folder-dispatcher-and-handler "/static/" (resource-path "static"))
  ))


;;; Database
(defclass login ()
  ((id :col-type serial :accessor id)
   (fullname :col-type string :initarg :fullname :accessor user-fullname)
   (username :col-type string :initarg :username :accessor user-username)
   (password :col-type string :initarg :password :accessor user-password))
  (:metaclass postmodern:dao-class)
  (:keys id))

(defclass email ()
  ((id :col-type serial :accessor id)
   (user-id :col-type integer :initarg :user-id :accessor email-user-id)
   (email :col-type string :initarg :email :accessor email-address)
   (primary :col-type boolean :initform nil
            :initarg :primary :accessor email-primary)
   (verified :col-type boolean  :initform nil
             :initarg :verified :accessor email-verified))
  (:metaclass postmodern:dao-class)
  (:keys id user-id))

(defclass keys ()
  ((id :col-type serial :accessor id)
   (user-id :col-type integer :initarg :user-id :accessor keys-user-id)
   (title :col-type string :initarg :title :accessor key-title)
   (type :col-type string :initarg :type :accessor key-type)
   (key :col-type string :initarg :key :accessor key-value))
  (:metaclass postmodern:dao-class)
  (:keys id user-id))

(defclass repository ()
  ((id :col-type serial :accessor id)
   (owner-id :col-type integer :initarg :owner-id)
   (name :col-type string :initarg :name :accessor repository-name)
   (path :col-type string :initarg :path :accessor repository-path)
   (branch :col-type (or postmodern:db-null string) :initarg :branch
           :accessor repository-branch)
   (public :col-type boolean :initarg :public :accessor repository-public))
  (:metaclass postmodern:dao-class)
  (:keys id))


(defmethod repository-real-path ((repo repository))
  (merge-pathnames (slot-value repo 'path)
		   *repository-directory*))


(defmethod user-primary-email ((user login))
  (car (postmodern:select-dao 'email (:and (:= 'user-id (id user)) (:= 'primary t)))))


(defun create-tables ()
  (unless (postmodern:table-exists-p 'login)
    (postmodern:execute (postmodern:dao-table-definition 'login)))
  (unless (postmodern:table-exists-p 'email)
    (postmodern:execute (postmodern:dao-table-definition 'email)))
  (unless (postmodern:table-exists-p 'keys)
    (postmodern:execute (postmodern:dao-table-definition 'keys)))
  (unless (postmodern:table-exists-p 'repository)
    (postmodern:execute (postmodern:dao-table-definition 'repository))))


;;; Path


(defun remove-ref-path (ref &optional (substring "refs/heads/"))
  "remove a substring from the start of a string"
  (let ((location (search substring ref)))
    (string-trim " "
		 (if location
		     (subseq ref (length substring))
		     ref))))


;;; View


(defun gravatar-url (email &key (size 80))
    (concatenate 'string
		 "http://www.gravatar.com/avatar/"
		 (format nil "~(~{~2,'0X~}~)"
			 (map 'list #'identity (md5:md5sum-sequence (coerce email 'simple-string))))
		 "?s="
		 (prin1-to-string size)))


(defun url-join (&rest rest)
  (let ((sequence (mapcan #'(lambda (x) (list (string x) "/")) rest)))
    (reduce #'(lambda (current next)
		(if (stringp next)
		    (concatenate 'string current next)
		    current))
	    sequence
	    :initial-value "/")))

(defun selected-branch (repository repository-branches url-branch)
  "From a REPOSITORY orm object a list of the git REPOSTIORY-BRANCHES
and the possible branch in the url (URL-BRANCH) return the most
aproprate branch to display."
  (let ((default-branch (slot-value repository 'branch))
	(default-branch* "refs/heads/master"))
    (cond
      ((eq repository-branches nil)
       nil)
      ((find url-branch repository-branches :test #'equal)
       url-branch)
      ((find default-branch repository-branches :test #'equal)
       default-branch)
      ((find default-branch* repository-branches :test #'equal)
       default-branch*)
      (t
       (car repository-branches)))))


(defun compare-password-hash (passwordhash password)
  (if (string= passwordhash password)
      T
      nil))


(defun verify-password (login password)
  (let* ((user (car (postmodern:query
		     (:select 'login.id 'login.password
			      :from 'login
			      :left-join 'email :on (:= 'login.id 'email.user-id)
			      :where (:or (:= 'login.username login) (:= 'email.email login))))))
	 (user-id (car user))
	 (user-passwd (car (cdr user))))
    (if (compare-password-hash user-passwd password)
	user-id
	nil)))


(defun login-session (login password)
  "log a user out of a session"
  (let ((user-id (verify-password login password)))
    (if user-id
	(let ((session (hunchentoot:start-session))
	      (user (postmodern:get-dao 'login user-id)))
	  (setf (hunchentoot:session-value 'user session) user)
	  )
	nil
	)))


(defun logout-session ()
  "remove the user from the current session-login"
  (hunchentoot:delete-session-value 'user))


(defun loginp ()
  (hunchentoot:session-value 'user))


(defun create-repository (name owner public)
  (let* ((username  (slot-value owner 'username))
	 (relative-path (make-pathname :directory
					       (list ':relative
						     (string username)
						     (string name))))
	 (path (merge-pathnames relative-path
			       *repository-directory*)))
    (ensure-directories-exist path)
    (postmodern:insert-dao
     (make-instance 'repository
		    :owner-id (slot-value owner 'id)
		    :name name
		    :path (namestring relative-path)
		    :public public))
    (cl-git:ensure-git-repository-exist path t)))


(defun create-user (username fullname password email)
  "Create a new user from the attributes USERNAME FULLNAME PASSWORD
and set the primary email address to EMAIL"
  (postmodern:with-transaction ()
    (let ((login (postmodern:insert-dao
                  (make-instance 'login
                                 :fullname fullname
                                 :username username
                                 :password password))))
      (postmodern:insert-dao
       (make-instance 'email
                      :user-id (id login)
                      :email email
                      :primary t))
      login)))
