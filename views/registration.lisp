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

;;;; registration.lisp

(in-package #:planet-git)


(hunchentoot:define-easy-handler
    (register-page :uri "/register")
    ((fullname :parameter-type 'string :request-type :post)
     (username :parameter-type 'string :request-type :post)
     (password :parameter-type 'string :request-type :post)
     (cpassword :parameter-type 'string :request-type :post)
     (email :parameter-type 'string :request-type :post))
  (let ((errors (validate-registration)))
    (if (and (= (hash-table-count errors) 0)
	     (eq (hunchentoot:request-method*) :post))
	(progn
	  (let ((login (postmodern:insert-dao
			(make-instance 'login
				       :fullname fullname
				       :username username
				       :email email
				       :password password)))
		(session (hunchentoot:start-session)))
	    (postmodern:insert-dao
	     (make-instance 'email
			    :user-id (slot-value login 'id)
			    :email email
			    :rank 0))
	    (setf (hunchentoot:session-value 'user session) login)
	    (hunchentoot:redirect (url-join (slot-value login 'username)))))
	(render-standard-page (:title "Register")
	  (:form :action "" :method "post"
		 (if (> (hash-table-count errors) 0)
		     (cl-who:htm
		      (:div :class "alert-message error"
			    (:p "Error detected on the page"))))
		 (field-fragment "fullname" "Fullname:" "text"
			:value fullname
			:error (gethash 'fullname errors))
		 (field-fragment "username" "Username:" "text"
			:value username
			:error (gethash 'username errors))
		 (field-fragment "email" "Email:" "text"
			:value email
			:error (gethash 'email errors))
		 (field-fragment "password" "Password:" "text"
			:error (gethash 'password errors))
		 (field-fragment "cpassword" "confirm passwd" "text"
			:error (gethash 'cpassword errors))
		 (:div :class "actions"
		       (:input :class "btn primary" :type "submit"
			       :name "register" :value "Register"))))
	  )))


(hunchentoot:define-easy-handler
    (register-page :uri "/register")
    ((fullname :parameter-type 'string :request-type :post)
     (username :parameter-type 'string :request-type :post)
     (password :parameter-type 'string :request-type :post)
     (cpassword :parameter-type 'string :request-type :post)
     (email :parameter-type 'string :request-type :post))
  (let ((errors (validate-registration)))
    (if (and (= (hash-table-count errors) 0)
	     (eq (hunchentoot:request-method*) :post))
	(progn
	  (let ((login (postmodern:insert-dao
			(make-instance 'login
				       :fullname fullname
				       :username username
				       :email email
				       :password password)))
		(session (hunchentoot:start-session)))
	    (postmodern:insert-dao
	     (make-instance 'email
			    :user-id (slot-value login 'id)
			    :email email
			    :rank 0))
	    (setf (hunchentoot:session-value 'user session) login)
	    (hunchentoot:redirect (url-join (slot-value login 'username)))))
	(render-standard-page (:title "Register")
	  (:form :action "" :method "post"
		 (if (> (hash-table-count errors) 0)
		     (cl-who:htm
		      (:div :class "alert-message error"
			    (:p "Error detected on the page"))))
		 (field-fragment "fullname" "Fullname:" "text"
			:value fullname
			:error (gethash 'fullname errors))
		 (field-fragment "username" "Username:" "text"
			:value username
			:error (gethash 'username errors))
		 (field-fragment "email" "Email:" "text"
			:value email
			:error (gethash 'email errors))
		 (field-fragment "password" "Password:" "text"
			:error (gethash 'password errors))
		 (field-fragment "cpassword" "confirm passwd" "text"
			:error (gethash 'cpassword errors))
		 (:div :class "actions"
		       (:input :class "btn primary" :type "submit"
			       :name "register" :value "Register"))))
	  )))


(hunchentoot:define-easy-handler
    (register-page :uri "/register")
    ((fullname :parameter-type 'string :request-type :post)
     (username :parameter-type 'string :request-type :post)
     (password :parameter-type 'string :request-type :post)
     (cpassword :parameter-type 'string :request-type :post)
     (email :parameter-type 'string :request-type :post))
  (let ((errors (validate-registration)))
    (if (and (= (hash-table-count errors) 0)
	     (eq (hunchentoot:request-method*) :post))
	(progn
	  (let ((login (postmodern:insert-dao
			(make-instance 'login
				       :fullname fullname
				       :username username
				       :email email
				       :password password)))
		(session (hunchentoot:start-session)))
	    (postmodern:insert-dao
	     (make-instance 'email
			    :user-id (slot-value login 'id)
			    :email email
			    :rank 0))
	    (setf (hunchentoot:session-value 'user session) login)
	    (hunchentoot:redirect (url-join (slot-value login 'username)))))
	(render-standard-page (:title "Register")
	  (:form :action "" :method "post"
		 (if (> (hash-table-count errors) 0)
		     (cl-who:htm
		      (:div :class "alert-message error"
			    (:p "Error detected on the page"))))
		 (field-fragment "fullname" "Fullname:" "text"
			:value fullname
			:error (gethash 'fullname errors))
		 (field-fragment "username" "Username:" "text"
			:value username
			:error (gethash 'username errors))
		 (field-fragment "email" "Email:" "text"
			:value email
			:error (gethash 'email errors))
		 (field-fragment "password" "Password:" "text"
			:error (gethash 'password errors))
		 (field-fragment "cpassword" "confirm passwd" "text"
			:error (gethash 'cpassword errors))
		 (:div :class "actions"
		       (:input :class "btn primary" :type "submit"
			       :name "register" :value "Register")))))))


(hunchentoot:define-easy-handler
    (login-page :uri "/login")
    ((login :parameter-type 'string :request-type :post)
     (password :parameter-type 'string :request-type :post)
     (came-from :parameter-type 'string))
  (let* ((errors (when (and login password) (validate-login)))
	 (logged-in (when (and errors (= (hash-table-count errors) 0)) (login-session login password))))
    (unless (and errors (gethash 'password errors))
      (setf (gethash 'password errors) "Invalid password."))
    (if logged-in
	(hunchentoot:redirect came-from)
	(render-standard-page (:title "Login")
	  (:form :action "" :class "login-form form-stacked" :method "post"
		 (if (> (hash-table-count errors) 0)
		     (cl-who:htm
		      (:div :class "alert-message error"
			    (:p "Error detected on the page"))))
		  (:input :type "hidden" :name "came-from"
			  :value came-from)
		  (field-fragment "login" "Username or Email:" "text"
			       :value login
			       :error (gethash 'login errors))
		  (field-fragment "password" "Password:" "text"
			       :error (gethash 'password errors))
		 (:div :class "actions"
		       (:a :class "btn secondary"
			   :href came-from "Cancel")
		       (:input :class "btn primary"
			       :type "submit"
			       :name "login"
			       :value "Login")))))))


(hunchentoot:define-easy-handler
    (logout-page :uri "/logout") ()
  (logout-session)
  (hunchentoot:redirect "/"))
