;;;; planet-git.lisp

(in-package #:planet-git)

;;; "planet-git" goes here. Hacks and glory await!
;(use-package '(swank hunchentoot cl-who postmodern))

(require :swank)
(require :hunchentoot)
(require :cl-who)
(require :postmodern)
(require :cl-ppcre)


;;; Global Config

(defparameter *repository-directory* #p"/home/russell/tmp/planet-git/")

;;; Webserver

(defun resource-path (path)
  "looks up path relative to whereever this asdf system is installed.  Returns a truename"
  (truename (asdf:system-relative-pathname :planet-git path)))


(setq hunchentoot:*dispatch-table*
 (list
  (hunchentoot:create-regex-dispatcher "^/?$" 'home-page)
  (hunchentoot:create-regex-dispatcher "^/[^/]+/$" 'user-page)
  (hunchentoot:create-regex-dispatcher "^/[^/]+/[^/]+/$" 'repository-page)
  (hunchentoot:create-regex-dispatcher "^/[^/]+/[^/]+/[^/]+/$" 'repository-branch-page)
  'hunchentoot:dispatch-easy-handlers
  (hunchentoot:create-folder-dispatcher-and-handler "/static/" (resource-path "static"))))


;;; Database
(defclass login ()
  ((id :col-type serial :accessor id)
   (fullname :col-type string :initarg :fullname)
   (username :col-type string :initarg :username)
   (password :col-type string :initarg :password))
  (:metaclass postmodern:dao-class)
  (:keys id))

(defclass email ()
  ((id :col-type serial :accessor id)
   (user-id :col-type integer :initarg :user-id)
   (rank :col-type integer :initarg :rank)
   (email :col-type string :initarg :email))
  (:metaclass postmodern:dao-class)
  (:keys id user-id rank))

(defclass repository ()
  ((id :col-type serial :accessor id)
   (owner-id :col-type integer :initarg :owner-id)
   (name :col-type string :initarg :name)
   (path :col-type string :initarg :path)
   (branch :col-type string :initarg :branch)
   (public :col-type boolean :initarg :public))
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
  (when (= (length (hunchentoot:parameter fieldname)) 0)
    (concatenate 'string "Error, " fieldname " is required")))

(defun validate-username (fieldname)
  (when (car (list (cl-ppcre:scan "[^a-zA-Z]" (hunchentoot:parameter fieldname))))
    (concatenate 'string "Error, " fieldname " can only contain alpha characters.")))

(defun validate-email (fieldname)
  (when (car (list (cl-ppcre:scan "^[^@]+@[^@]+\\.[^@]+$" (hunchentoot:parameter fieldname))))
    (concatenate 'string "Error, " fieldname " is not a valid email address.")))

;; FIXME (russell) this still doesn't work
(defun validate-password (fieldname)
  (when (equal (hunchentoot:parameter fieldname) (hunchentoot:parameter "password"))
    (concatenate 'string "Error, " fieldname " doesn't match password.")))

(defmacro validate-field (fieldname errors &rest validators)
  `(let ((lname ,fieldname)
	 (lerrors ,errors))
     (loop for x in (list ,@validators)
	   until (gethash lname lerrors)
	   do (let ((validation-error (funcall x (string-downcase (string lname)))))
		(unless (= (length validation-error) 0)
		  (setf (gethash lname lerrors) validation-error))))))

(defmacro def-validator (name () &body body)
  `(defun ,name ()
     (let ((errors (make-hash-table)))
       (if (eq (hunchentoot:request-method*) :post)
	   (progn
	     ,@body))
       errors)))

(def-validator validate-registration ()
  (validate-field 'fullname errors #'validate-length)
  (validate-field 'username errors #'validate-length #'validate-username)
  (validate-field 'email errors #'validate-length #'validate-email)
  (validate-field 'password errors #'validate-length)
  (validate-field 'cpassword errors #'validate-password))

(def-validator validate-login ()
  (validate-field 'login errors #'validate-length)
  (validate-field 'password errors #'validate-length))

(def-validator validate-newrepository ()
  (validate-field 'name errors #'validate-length))

;;; Path

(defun repository-path (repository)
  (merge-pathnames (slot-value repository 'path)
		   *repository-directory*))

(defun remove-ref-path (ref &optional (substring "refs/heads/"))
  "remove a substring from the start of a string"
  (let ((location (search substring ref)))
    (string-trim " "
		 (if location
		     (subseq ref (length substring))
		     ref))))

;;; View


(defmacro render-standard-page ((&key title (subtitle "") page-header) &body body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
     (:html :xmlns "http://www.w3.org/1999/xhtml"
	    :xml\:lang "en"
	    :lang "en"
	    (:head
	     (:meta :http-equiv "Content-Type"
		    :content    "text/html;charset=utf-8")
	     (:title "Planet Git - " ,title)
	     (:link :rel "stylesheet" :href "/static/css/bootstrap.css")
	     (:script :type "text/javascript" :src "/static/js/jquery.js")
	     (:script :type "text/javascript" :src "/static/js/bootstrap-modal.js")
	     (:style :type "text/css"
		     (cl-who:str
		      (css-lite:css
			(("html, body")
			 (:background-color "#eee"))
			((".container")
			 (:width "820px"))
			(("body")
			 (:padding-top "40px")) ; 40px to make the container go all the way to the bottom of the topbar
			((".container > footer p")
			 (:text-align "center")) ; center align it with the container

			((".container")
			 (:width "820px")); downsize our container to make the content feel a bit tighter and more cohesive. NOTE: this removes two full columns from the grid, meaning you only go to 14 columns and not 16.

					; The white background content wrapper
			((".content")
			 (:background-color "#fff"
			  :padding "20px"
			  :margin "0 -20px"; negative indent the amount of the padding to maintain the grid system
			  :-webkit-border-radius "0 0 6px 6px"
			  :-moz-border-radius "0 0 6px 6px"
			  :border-radius "0 0 6px 6px"
			  :-webkit-box-shadow "0 1px 2px rgba(0,0,0,.15)"
			  :-moz-box-shadow "0 1px 2px rgba(0,0,0,.15)"
			  :box-shadow "0 1px 2px rgba(0,0,0,.15)"))
					; Page header tweaks
			((".page-header")
			 (:background-color "#f5f5f5"
			  :padding "20px 20px 10px"
			  :margin "-20px -20px 20px"))
					; Styles you shouldn't keep as they are for displaying this base example only
			((".content .span10, .content .span4")
			 (:min-height "500px"))

					; Give a quick and non-cross-browser friendly divider
			((".content .span4")
			 (:margin-left "0"
			  :padding-left "19px"
			  :border-left "1px solid #eee"))

			 ((".topbar .btn")
			  (:border "0"))

			 (("ol.commit-list")
			   (:list-style-type "none")
			  (("li")
			   (:height "40px"
			    :margin "10px"
			    :padding-left "40px"))
			  (("p")
			   (:margin "10px"
			    :margin "5px"
			    :font-weight "bold"))
			  ((".author")
			   (:margin "5px"))
			  ((".date")
			   (:font-style "italic"
			    :font-size "smaller"))
			  (("img")
			   (:float "left"
			    :margin-left "-40px")))

			 ((".project-bar")
			  (:height "27px")
			  (("#branch")
			   (:float "right")))

			 ((".login-form") nil
			  (("ul")
			   (:list-style-type "none"))
			  (("input")
			   (:font-size "large"))
			  (("input.btn")
			   (:font-size "small")))
			 )))))
	    (:body
	     (:div :class "topbar"
		   (:div :class "fill"
			 (:div :class "container"
			       (:a :class "brand" :href "/" "Planet Git")
			       (:ul :class "nav")
			       (:ul :class "nav secondary-nav"
				    (if (loginp)
					(let ((username (slot-value (loginp) 'username)))
					  (cl-who:htm
					   (:li (:a :href (concatenate 'string "/" username "/") (cl-who:str username)))
					   (:li (:a :href "/logout" "Logout")))))
				    (unless (loginp)
				      (cl-who:htm
				       (modal ("login-modal"
					       "Login"
					       :buttons ((:a :href "#" :class "btn primary"
							     :onclick (ps:ps-inline
								       (ps:@
									(ps:chain ($ "#login-modal-form" )
										  (submit))))

							     "Login")
							 (:a :href "#" :class "btn secondary"
							     :onclick (ps:ps-inline
								       (ps:@
									(ps:chain ($ "#login-modal" )
										  (modal "hide"))))
							     "Cancel")))
					 (:form :id "login-modal-form" :class "login-form"
						:action "/login" :method "post"
						(:ul
						 (:input :type "hidden" :name "came-from"
							 :value (hunchentoot:request-uri*))
						 (:li "Username or Email:")
						 (:li (:input :type "text" :name "login"))
						 (:li "Password:")
						 (:li (:input :type "text" :name "password")))))
				       (:li (:a :href "/register" "Register"))
				       (:li (:a :href (concatenate 'string
								   "/login?came-from="
								   (hunchentoot:request-uri*))
						:data-controls-modal "login-modal"
						:data-backdrop "true"
						"Login"))))))))
	     (:div :class "container"
		   (:div :class "content"
			 (:div :class "page-header"
			       ,page-header
			       (:h1 ,title
				    (:small ,subtitle)))
			 (:div :class "row"
			       (:div :class "span14"
			       ,@body)))))))


(defmacro def-who-macro (name (&rest args) pseudo-html-form)
  "A macro for use with CL-WHO's WITH-HTML-OUTPUT."
    `(defmacro ,name (,@args)
       `(cl-who:with-html-output (*standard-output* nil)
	  ,,pseudo-html-form)))

(defmacro def-who-macro* (name (&rest args) pseudo-html-form)
  "Who-macro, which evaluates its arguments (like an ordinary function,
which it is in fact.
   Useful for defining syntactic constructs"
  `(defun ,name (,@args)
     ,pseudo-html-form))

(def-who-macro modal ((id heading &key buttons) &body body)
  (let ((buttons (if buttons buttons
		     '((:a :href "#" :class "btn primary" "Primary")
		       (:a :href "#" :class "btn secondary" "Secondary")))))
    `(:div :id ,id :class "modal hide fade"
		 (:div :class "modal-header"
		       (:a :href "#" :class "close" "&times;")
		       (:h3 ,heading))
		 (:div :class "modal-body"
		       ,@body)

		 (:div :class "modal-footer"
		       ,@buttons))))


(defun home-page ()
 (render-standard-page (:title "Planet Git" :subtitle "a bad clone of github")
    (if (loginp) (cl-who:htm (:a :href "/repository/new" "new repository")))))


(defun user-page ()
  (let*
      ((req (hunchentoot:request-uri*))
       (username (cl-ppcre:register-groups-bind (username)
		 ("^/(\\w+)/?$" req)
	       username))
       (user (car (postmodern:select-dao 'login (:= 'username username)))))
    (if user
	(let ((username (slot-value user 'username)))
	  (render-standard-page (:title (cl-who:str username)
			  :subtitle (cl-who:str (slot-value user 'fullname))
			  :page-header (:a :class "btn primary pull-right"
					   :href "/repository/new"
					   "Add Repository"))
	    (let ((repositories (postmodern:select-dao
				 'repository (:= 'owner-id (slot-value user 'id)))))
	      (labels ((repository-fragment (repos)
			 (let ((repo (car repos)) (rest (cdr repos)))
			   (if repo
			       (cl-who:htm
				(:div :class "repository"
				      (:a :href (cl-who:str (concatenate
							     'string
							     (hunchentoot:request-uri*)
							     (slot-value repo 'name)
							     "/"))
					  (:h3 :class "name" (cl-who:str
							      (slot-value repo 'name))))))
			       (repository-fragment rest)))))
		(when repositories
		  (repository-fragment repositories))))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))))

(defun flatten (list)
  (cond
    ((null list) list)
    ((null (car list)) (flatten (cdr list)))
    ((atom (car list)) (cons (car list) (flatten (cdr list))))
    (t (append (flatten (car list)) (flatten (cdr list))))))

(defun gravatar-url (email &key (size 80))
  (concatenate 'string
	       "http://www.gravatar.com/avatar/"
	       (format nil "~(~{~2,'0X~}~)"
		       (map 'list #'identity (md5:md5sum-sequence email)))
	       "?s="
	       (prin1-to-string size)))

(defun repository-page ()
  (let*
      ((req (hunchentoot:request-uri*))
       (uri-parts (cl-ppcre:register-groups-bind
		      (username repository-name)
		      ("^/([^/]+)/([^/]+)/?$" req)
		    (list username repository-name)))
       (username (car uri-parts))
       (repository-name (car (cdr uri-parts))))
    (repository username repository-name)))

(defun repository-branch-page ()
  (let*
      ((req (hunchentoot:request-uri*))
       (uri-parts (cl-ppcre:register-groups-bind
		      (username repository-name branch)
		      ("^/([^/]+)/([^/]+)/([^/]+)/?$" req)
		    (list username repository-name branch)))
       (username (car uri-parts))
       (repository-name (second uri-parts))
       (branch (concatenate 'string "refs/heads/" (third uri-parts))))
    (repository username repository-name :branch branch)))

(defun url-join (&rest rest)
  (let ((sequence (mapcan #'(lambda (x) (list (string x) "/")) rest)))
    (reduce #'(lambda (current next)
		(if (stringp next)
		    (concatenate 'string current next)
		    current))
	    sequence
	    :initial-value "/")))

(defun repository (username repository-name &key branch)
  (let*
      ((user (car (postmodern:select-dao 'login (:= 'username username))))
       (repository (car (postmodern:select-dao
			 'repository (:and
				      (:= 'owner-id (slot-value user 'id))
				      (:= 'name repository-name)))))
       (branch (if branch branch (slot-value repository 'branch))))
    (if (and user repository)
	(cl-git:with-git-repository ((repository-path repository))
	  (if (find branch (cl-git:git-reference-listall) :test #'equal)
	      (render-standard-page (:title (cl-who:str (slot-value repository 'name)))
		(cl-who:htm
		 (:script :type "text/javascript"
			  (cl-who:str
			   (ps:ps
			     (defun select-branch (branch)
			       (setf (ps:getprop window 'location 'href)
				     (concatenate 'string
						  (ps:lisp (url-join username repository-name))
						  branch "/"))))))
		 (:div :class "project-bar"
		       (:select :id "branch"
				:onchange (ps:ps-inline (select-branch
						  (ps:@ this options
							(ps:@ this selected-index) value)))
				(mapcar #'(lambda (x)
					    (cl-who:htm
					     (:option
					      :value (remove-ref-path x)
					      :selected (when (equal x branch) "true")
					      (cl-who:str (remove-ref-path x)))))
					(cl-git:git-reference-listall))))
		 (:ol :class "commit-list"
		      (let ((count 0))
			(cl-git:with-git-revisions (commit :head branch)
			  (setf count (+ count 1))
			  (when (> count 10) (return))
			  (cl-who:htm
			   (:li
			    (let* ((author (cl-git:git-commit-author commit))
				   (name (first author))
				   (email (second author))
				   (timestamp (third author)))
			      (cl-who:htm
			       (:img :src (gravatar-url email :size 40))
			       (:p
				(cl-who:str
				 (cl-git:git-commit-message commit)))
			       (:span :class "author" (cl-who:str name))
			       (:span :class "date"
				      (cl-who:str
				       (local-time:format-timestring nil timestamp :format
								     '(:long-month " " :day ", " :year))))))
			    )))))))
	      (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)))
	  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))))


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
	(render-standard-page (:title "Register")
	  (:form :action "" :method "post"
		 (if (> (hash-table-count errors) 0)
		     (cl-who:htm
		      (:div :class "alert-message error"
			    (:p "Error detected on the page"))))
		 (field "fullname" "Fullname:" "text"
			:value fullname
			:error (gethash 'fullname errors))
		 (field "username" "Username:" "text"
			:value username
			:error (gethash 'username errors))
		 (field "email" "Email:" "text"
			:value email
			:error (gethash 'email errors))
		 (field "password" "Password:" "text"
			:error (gethash 'password errors))
		 (field "cpassword" "confirm passwd" "text")
		 (:div :class "actions"
		       (:input :class "btn primary" :type "submit"
			       :name "register" :value "Register"))))
	  ))))


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

(def-who-macro field (name description type &key value error)
  `(:div :class (if ,error "clearfix error" "clearfix")
	(:label ,description)
	(:div :class "input"
	      (:input :type ,type :name ,name
		      :class (if ,error "error")
		      :value ,value)
	      (:span :class "help-inline" (cl-who:str ,error)))))

(hunchentoot:define-easy-handler
    (login-page :uri "/login")
    ((login :parameter-type 'string :request-type :post)
     (password :parameter-type 'string :request-type :post)
     (came-from :parameter-type 'string))
  (let* ((errors (validate-login))
	 (logged-in (when (= (hash-table-count errors) 0) (login-session login password))))
    (unless (gethash 'password errors)
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
		  (field "login" "Username or Email:" "text"
			       :value login
			       :error (gethash 'login errors))
		  (field "password" "Password:" "text"
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


(defun create-repository (name owner)
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
		    :branch "refs/heads/master"
		    :public nil))
    (cl-git:ensure-git-repository-exist path t)))

(hunchentoot:define-easy-handler
    (new-repository-page :uri "/repository/new")
    ((name :parameter-type 'string))
  (let* ((errors (validate-newrepository)))
    (if (and (= (hash-table-count errors) 0)
	     (eq (hunchentoot:request-method*) :post))
	(progn
	  (create-repository name (loginp))
	  (hunchentoot:redirect (concatenate 'string "/"
					     (slot-value (loginp) 'username) "/"name "/")))
      (render-standard-page (:title "New Repository")
	(:form :action "" :method "post" :class "form-stacked"
	       (if (> (hash-table-count errors) 0)
		   (cl-who:htm
		    (:div :class "alert-message error"
			  (:p "Error detected on the page"))))
	       (field "name" "Name:" "text"
		      :error (gethash 'name errors))
	       (:div :class "actions"
		     (:input :type "submit"
			     :class "btn primary"
			     :name "submit"
			     :value "Create")))))))
