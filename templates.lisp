;;;; templates.lisp

(in-package #:planet-git)


(defmacro def-who-macro (name (&rest args) &optional documentation pseudo-html-form)
  "A macro for use with CL-WHO's WITH-HTML-OUTPUT."
  (let ((documentation (if (stringp documentation) documentation ""))
	(pseudo-html-form (if (stringp documentation) pseudo-html-form documentation)))
    `(defmacro ,name (,@args)
       ,documentation
       `(cl-who:with-html-output (*standard-output* nil)
	  ,,pseudo-html-form))))

(defmacro def-who-macro* (name (&rest args) &optional documentation pseudo-html-form)
  "Who-macro, which evaluates its arguments (like an ordinary function,
which it is in fact.  Useful for defining syntactic constructs"
  (let ((documentation (if (stringp documentation) documentation ""))
	(pseudo-html-form (if (stringp documentation) pseudo-html-form documentation)))
    `(defun ,name (,@args)
       ,documentation
       (cl-who:with-html-output (*standard-output* nil)
	 ,pseudo-html-form))))

(defmacro render-standard-page ((&key title (subtitle "") (body-class "span14") page-header) &body body)
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
			  :margin "-20px -20px 20px")
			 (("h1")
			 (:display "inline"
			  :vertical-align "top"))
			 (("span")
			  (:margin ("0 5px")))
			 (("small")
			  (:margin ("0 5px")))
			 (("img")
			  (:margin-right "5px")))
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

			 ((".project") nil
			  (("span.private")
			   (:color "red")))

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
					   (:li (:a :href (url-join username) (cl-who:str username)))
					   (:li (:a :href (url-join username "settings") (cl-who:str "Settings")))
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
						 (:li (:input :type "text" :name "password")))
						(:input :type "submit"
							:style "visibility: hidden;"
							:name "create"
							:value "Create")))
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
			       ,(or `(cl-who:htm ,page-header)
				    `(cl-who:htm (:h1 ,title
						      (:small ,subtitle)))))
			 (:div :class "row"
			       (:div :class ,body-class
			       ,@body)))))))
