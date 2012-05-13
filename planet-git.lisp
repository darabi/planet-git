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

(defvar *repository-directory* #P"/tmp/")
(defvar *git-user-homedir* #P"/home/git/")
(defvar *git-shell-path* #P"/home/git/")
(defvar *git-ssh-host* "git@localhost")

;;; Webserver

(defun resource-path (path)
  "looks up path relative to whereever this asdf system is installed.  Returns a truename"
  (truename (asdf:system-relative-pathname :planet-git path)))

(setq *rest-handler-alist*
      (list
       (list "^/?$" t 'home-page)
       (list "^/register?$" t 'register-page)
       (list "^/(\\w+)/settings/?$" t 'user-settings-page)
       (list "^/(\\w+)/settings/email/(\\w+)/delete/?$" t 'user-email-delete)
       (list "^/(\\w+)/settings/key/(\\w+)/delete/?$" t 'user-key-delete)
       (list "^/(\\w+)/settings/add-key?$" t 'add-ssh-key)
       (list "^/[^/]+/$" t 'user-page)
       (list "^/[^/]+/[^/]+/$" t 'repository-home-page)
       (list "^/[^/]+/[^/]+/key/[^/]+/$" t 'repository-key-access)
       (list "^/[^/]+/[^/]+/branch/[^/]+/$" t 'repository-branch-page)))

(setq hunchentoot:*dispatch-table*
 (list
  #'hunchentoot:dispatch-easy-handlers
  #'dispatch-rest-handlers
  (hunchentoot:create-folder-dispatcher-and-handler "/static/" (resource-path "static"))
  ))


;;; Database
(defclass login ()
  ((id :col-type serial :accessor id)
   (fullname :col-type string :initarg :fullname :accessor user-fullname)
   (location :col-type (or postmodern:db-null string)
             :initarg :location :accessor user-location)
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

(defclass key ()
  ((id :col-type serial :accessor id)
   (user-id :col-type integer :initarg :user-id :accessor key-user-id)
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
   (branch :col-type (or postmodern:db-null string)
           :initarg :branch
           :accessor repository-branch)
   (description :col-type (or postmodern:db-null string)
                :initarg :description
                :accessor repository-description)
   (public :col-type boolean :initarg :public :accessor repository-public))
  (:metaclass postmodern:dao-class)
  (:keys id))


(defmethod repository-real-path ((repo repository))
  (merge-pathnames (slot-value repo 'path)
		   *repository-directory*))


(defmethod user-primary-email ((user login))
  (car (postmodern:select-dao 'email (:and (:= 'user-id (id user)) (:= 'primary t)))))

(defmethod user-gravatar-url ((user login) &key (size 80))
  "return the url to a USER's gravatar, an optional SIZE keyword can
be used to set the requested size."
  (gravatar-url (email-address (user-primary-email user)) :size size))

(defun create-tables ()
  (unless (postmodern:table-exists-p 'login)
    (postmodern:execute (postmodern:dao-table-definition 'login)))
  (unless (postmodern:table-exists-p 'email)
    (postmodern:execute (postmodern:dao-table-definition 'email)))
  (unless (postmodern:table-exists-p 'key)
    (postmodern:execute (postmodern:dao-table-definition 'key)))
  (unless (postmodern:table-exists-p 'repository)
    (postmodern:execute (postmodern:dao-table-definition 'repository))))

(defun key-parse (key)
  "parse a KEY string and return a new KEYS instance, if there is a
  current user then set them as the foreign key."
  (cl-ppcre:register-groups-bind
      (type key title)
      ("^(\\S*)\\s+(\\S*)\\s+(\\S*)$" key)
    (eval
     `(make-instance 'key :type ,type :key ,key :title ,title
                     ,@(when (loginp) (list :user-id (id (loginp))))))))

(defmethod key-to-authorizedkeys ((key key))
  "Add the users authorized KEY to the authorized_keys file."
  (with-open-file (stream (merge-pathnames ".ssh/authorized_keys" *git-user-homedir*)
                          :direction :output
                          :if-exists :append)
    (format stream
            "command=\"KEY_ID=~A ~A\",no-port-forwarding,no-agent-forwarding,no-X11-forwarding,no-pty ~A ~A ~A~%"
            (id key) *git-shell-path* (key-type key) (key-value key) (key-title key))))

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
  "return the gravatar url for an EMAIL address, an optional SIZE
keyword can be used to set the requested size."
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
  "log a user in to a session, the user object will be stored as the
value of the session."
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
  "if there is a current session then reurn its value which will be a
user object."
  (when (boundp 'hunchentoot:*session*)
    (hunchentoot:session-value 'user)))


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
