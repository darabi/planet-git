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

;;;; user.lisp

(in-package #:planet-git)


(def-who-macro repository-item-fragment (name owner public)
  `(cl-who:htm
    (:div :class "well project"
          (:a :href (cl-who:str (url-join ,owner ,name))
              (:h3 :class "name"
                   (cl-who:str ,name)))
          (unless ,public
            (cl-who:htm (:span :class "label label-important" "Private"))))))


(define-rest-handler (user-page :uri "^/(\\w+)/?$" :args (username)) ()
  (let
      ((user (car (postmodern:select-dao 'login (:= 'username username)))))
    (if user
	(let ((username (slot-value user 'username))
	      (is-current-user (equal (slot-value user 'username)
				      (when (loginp) (slot-value (loginp) 'username)))))
	  (render-user-page
          (user
           :extra-header (when is-current-user
                           (cl-who:htm (:a :class "btn primary pull-right"
                                           :href "/repository/new"
                                           "Add Repository"))))
	    (let ((repositories (postmodern:select-dao
				 'repository (:= 'owner-id (slot-value user 'id)))))
	      (hunchentoot:log-message* hunchentoot:*lisp-warnings-log-level* "Repositories ~a" repositories)
	      (labels ((repository-fragment (repos)
			 (let* ((repo (car repos)) (rest (cdr repos))
				(visible (or (slot-value repo 'public)
					     (equal (slot-value user 'username)
						    (when (loginp) (slot-value (loginp) 'username)))))
				(public (repository-public repo)))
			   (hunchentoot:log-message* hunchentoot:*lisp-warnings-log-level* "Repository ~a" repo)
			   (when (and repo (or visible is-current-user))
			     (repository-item-fragment (slot-value repo 'name)
						       username
						       public))
			   (when rest (repository-fragment rest)))))
		(when repositories (repository-fragment repositories))))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))))


(def-who-macro* email-item-fragment (user email)
  "this fragment renders a users email address as a list item with a
delete button"
  (cl-who:htm
   (:tr
    (:td
	 (:a :class "close"
         :href (cl-who:str
                (url-join (slot-value user 'username)
                          "settings"
                          "email"
                          (write-to-string (slot-value email 'id))
                          "delete"))
	     (cl-who:str "x"))
	 (cl-who:str (slot-value email 'email))))))


(def-who-macro* user-settings-page (user emails)
		(render-standard-page (:title (cl-who:str (user-username user))
				       :page-header
				       ((:img :src (user-gravatar-url user :size 40))
                        (:h1 (:a :href (url-join (user-username user))
                                 (cl-who:str (user-username user)))
					     (:small "Settings"))))
          (:h2 "Personal Information")
		  (form-fragment login-form
				 (('fullname "Fullname:" "text" :value (user-fullname user)))
                 :class "well form-stacked"
				 :buttons ((:input :type "submit"
						   :class "btn primary"
						   :name "login-form-submit"
						   :value "Save")))
          (:h2 "Emails")
          (:table :class "table"
		  (labels ((email-fragment (emails)
			     (let* ((email (car emails)) (rest (cdr emails)))
			       (email-item-fragment user email)
			       (when rest (email-fragment rest)))))
		    (when emails (email-fragment emails))))
		  (form-fragment email-form
				 (('email "Email:" "text"))
                 :class "well form-inline"
				 :buttons ((:input :type "submit"
						   :class "btn primary"
						   :name "email-form-submit"
						   :value "Add")))))


(define-form-handler (user-settings-view :uri "^/(\\w+)/settings/?$"
                                         :args (username))
    ((email-form
      (email :parameter-type 'string :request-type :post
             :validate (#'validate-length #'validate-email)))
     (login-form
      (fullname :parameter-type 'string :request-type :post
                :validate (#'validate-length))))
  (let*
      ((user (car (postmodern:select-dao 'login (:= 'username username))))
       (is-current-user (when user
                          (equal
                           (slot-value user 'username)
                           (when (loginp)
                             (slot-value (loginp) 'username))))))
    (if is-current-user
        (progn
          (setf *current-form*
                (cond-forms
                 (email-form
                  (postmodern:insert-dao
                   (make-instance 'email
                                  :user-id (slot-value (loginp) 'id)
                                  :email email)))
                 (login-form
                  (let ((user (postmodern:get-dao 'login (slot-value user 'id))))
                    (setf (slot-value user 'fullname) fullname)
                    (postmodern:update-dao user)))))
          (let ((emails (postmodern:select-dao 'email (:= 'user-id (slot-value user 'id)))))
            (user-settings-page user emails)))
        (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+))))


(define-rest-handler (user-email-delete :uri "^/(\\w+)/settings/email/(\\w+)/delete/?$" :args (username email-id)) ()
  (let*
      ((user (car (postmodern:select-dao 'login (:= 'username username))))
       (is-current-user (when user
			  (equal
			   (slot-value user 'username)
			   (when (loginp)
			     (slot-value (loginp) 'username))))))
    (if is-current-user
	(let ((email (car
		      (postmodern:select-dao 'email
					     (:and (:= 'id email-id) (:= 'user-id (id user)))))))
	  (if email
	      (postmodern:delete-dao email)
	      (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))
	  (hunchentoot:redirect (url-join (slot-value user 'username) "settings")))
	(setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+))))


(define-rest-handler (add-ssh-key
                      :uri "^/(\\w+)/settings/add-key?$"
                      :args (username))
    ()
  (let*
      ((user (car (postmodern:select-dao 'login (:= 'username username))))
       (is-current-user (when user (equal (slot-value user 'username)
					  (when (loginp) (slot-value (loginp) 'username))))))
					;    (if is-current-user)
    ))
