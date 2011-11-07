;;;; planet-git.lisp

(in-package #:planet-git)

;;; "planet-git" goes here. Hacks and glory await!
;(use-package '(swank hunchentoot cl-who postmodern))

(require :swank)
(require :hunchentoot)
(require :cl-who)
(require :postmodern)
(require :cl-ppcre)

;;; Database
(defclass login ()
  ((id :col-type serial :accessor id)
   (fullname :col-type string :initarg :fullname)
   (username :col-type string :initarg :username)
   (password :col-type string :initarg :password)
   )
  (:metaclass postmodern:dao-class)
  (:keys id))

(defclass email ()
  ((id :col-type serial :accessor id)
   (user-id :col-type integer :initarg :user-id)
   (rank :col-type integer :initarg :rank)
   (email :col-type string :initarg :email)
   )
  (:metaclass postmodern:dao-class)
  (:keys id user-id rank))

(defclass repository ()
  ((id :col-type serial :accessor id)
   (owner-id :col-type integer :accessor owner-id)
   (name :col-type string :initarg :name)
   )
  (:metaclass postmodern:dao-class)
  (:keys id))


(postmodern:connect-toplevel "planet_git" "gitui" "oenRTe90u" "localhost")

(unless (postmodern:table-exists-p 'login)
  (postmodern:execute (postmodern:dao-table-definition 'login)))
(unless (postmodern:table-exists-p 'email)
  (postmodern:execute (postmodern:dao-table-definition 'email)))
(unless (postmodern:table-exists-p 'repository)
  (postmodern:execute (postmodern:dao-table-definition 'repository)))


;;; Validation


(defun validate-length (fieldname)
     (if (= (length (hunchentoot:parameter fieldname)) 0)
       (concatenate 'string "Error, " fieldname " is required")))

(defun validate-username (fieldname)
  (if (car (list (cl-ppcre:scan "[^a-zA-Z]" (hunchentoot:parameter fieldname))))
       (concatenate 'string "Error, " fieldname " can only contain alpha characters.")))

(defmacro validate-field (fieldname errors &rest validators)
    `(let ((lname ,fieldname)
	    (lerrors ,errors))
       (loop for x in (list ,@validators)
	  until (gethash lname lerrors)
	  do (let ((validation-error (funcall x (string-downcase (string lname)))))
	       (unless (= (length validation-error) 0)
		 (setf (gethash lname lerrors) validation-error)
		 )))))

(defun validate-registration ()
  (let ((errors (make-hash-table)))
    (if (eq (hunchentoot:request-method*) :post)
	(progn 
	  (validate-field 'fullname errors #'validate-length)
	  (validate-field 'username errors #'validate-length #'validate-username)))
    errors
))

(defun validate-login ()
  (let ((errors (make-hash-table)))
    (if (eq (hunchentoot:request-method*) :post)
	(progn 
	  (validate-field 'username errors #'validate-length)
	  (validate-field 'password errors #'validate-length)))
    errors
))


;;; View          

;(setf hunchentoot:*dispatch-table*
;      (list #'hunchentoot:dispatch-easy-handlers))
;            #'hunchentoot:default-dispatcher))

(defmacro standard-page ((&key title) &body body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml"
	    :xml\:lang "en" 
	    :lang "en"
	    (:head 
	     (:meta :http-equiv "Content-Type" 
		    :content    "text/html;charset=utf-8")
	     (:title "Planet Git - " ,title))
	    (:body 
	     (:div :id "header"
		   (:h1 "Planet Git")
		   (:span :class "tagline" 
			  "a bad clone of github"))
	     ,@body))))


(hunchentoot:define-easy-handler 
    (home-page :uri "/") ()  
 (standard-page (:title "Home")
    (:a :href "/register" "register")
    (:a :href "/login" "login")
    (:a :href "/repository/new" "new repository")
    ))


(hunchentoot:define-easy-handler
    (register-page :uri "/register") 
    ((fullname :parameter-type 'string :request-type :post)
     (username :parameter-type 'string :request-type :post)
     (password :parameter-type 'string :request-type :post)
     (email :parameter-type 'string :request-type :post))
  (let ((errors (validate-registration)))
    (if (and (= (hash-table-count errors) 0) 
	     (eq (hunchentoot:request-method*) :post))
	(progn 
	  (postmodern:insert-dao 
	   (make-instance 'email
			  :user-id (postmodern:insert-dao 
				    (make-instance 'login
						   :fullname fullname
						   :username username
						   :password password))
			  :email email
			  :rank 0))
	  (hunchentoot:redirect "/"))
	(standard-page (:title "Register")
	  (:h1 "Register")
	  (:form :action "" :method "post" 
		 (:ul
		  (:li "Fullname" (:input :type "text" :name "fullname" :value fullname)
		       (cl-who:str (gethash 'fullname errors)))
		  (:li "Username" (:input :type "text" :name "username" :value username)
		       (cl-who:str (gethash 'username errors)))
		  (:li "Email" (:input :type "text" :name "email" :value email))
		  (:li "password" (:input :type "text" :name "password"))
		  (:li "confirm passwd" (:input :type "text" :name "cpassword"))
		  (:li (:input :type "submit" :name "register"))))
	  ))))


(hunchentoot:define-easy-handler
    (register-page :uri "/login") 
    ((login :parameter-type 'string) 
     (password :parameter-type 'string))
  (if login
      (progn 
	(hunchentoot:redirect "/"))
      (cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
	(:html 
	 (:body
	  (:h1 "Register")
	  (:form :action "" :method "post" 
		 (:ul
		  (:li "username or email" (:input :type "text" :name "fullname"))
		  (:li "password" (:input :type "text" :name "password"))
		  (:li (:input :type "submit" :name "login"))))
	  )))))


(hunchentoot:define-easy-handler
    (repository-page :uri "/repository/new") 
    ((name :parameter-type 'string))
  (if name
      (progn 
	(postmodern:insert-dao 
	 (make-instance 'repository
			:name name))
	(hunchentoot:redirect "/"))
      (cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
	(:html 
	 (:body
	  (:h1 "New Repository")
	  (:form :action "" :method "post" 
		 (:ul
		  (:li "Name" (:input :type "text" :name "name"))
		  (:li (:input :type "submit" :name "submit"))))
	  )))))
