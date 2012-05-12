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


(define-form-handler (register-page :uri "^/register$")
    ((new-user-form
      (fullname :parameter-type 'string :request-type :post
                :validate (#'validate-length))
      (username :parameter-type 'string :request-type :post
                :validate (#'validate-length
                           #'validate-username
                           #'validate-username-exists))
      (password :parameter-type 'string :request-type :post
                :validate (#'validate-length))
      (cpassword :parameter-type 'string :request-type :post
                 :validate (#'validate-password))
      (email :parameter-type 'string :request-type :post
             :validate (#'validate-length #'validate-email))))
  (if-valid-form
   (let ((login (create-user username fullname password email))
         (session (start-session)))
     (setf (session-value 'user session) login)
     (redirect (url-join (user-username login))))
   (render-standard-page (:title "Register")
     (form-fragment
      new-user-form
      (('fullname "Fullname:" "text")
       ('username "Username:" "text")
       ('email "Email:" "text")
       ('password "Password:" "password")
       ('cpassword "confirm passwd" "password"))
      :buttons ((:input :class "btn primary" :type "submit"
                        :name "new-user-form-submit" :value "Register"))))))


(define-easy-handler
    (login-page :uri "/login")
    ((login :parameter-type 'string :request-type :post)
     (password :parameter-type 'string :request-type :post)
     (came-from :parameter-type 'string))
  (let* ((errors (if (and login password) (validate-login)))
         (logged-in (when (and errors (= (hash-table-count errors) 0)) (login-session login password))))
    (if (and errors (gethash 'password errors))
      (setf (gethash 'password errors) "Invalid password.")
      (setf errors (make-hash-table)))
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
                 (field-fragment "password" "Password:" "password"
                                 :error (gethash 'password errors))
                 (:div :class "actions"
                       (:a :class "btn secondary"
                           :href came-from "Cancel")
                       (:input :class "btn primary"
                               :type "submit"
                               :name "login"
                               :value "Login")))))))


(define-easy-handler
    (logout-page :uri "/logout") ()
  (logout-session)
  (redirect "/"))
