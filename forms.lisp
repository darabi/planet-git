;;;; forms.lisp

(in-package #:planet-git)

(defparameter *form-errors* (make-hash-table))
(defparameter *form-data* (make-hash-table))
(defparameter *current-form* nil)

(defun compute-real-form-name (symbol)
  "Computes the `real' paramater name \(a string) from the Lisp symbol
SYMBOL.  Used in cases where no parameter name is provided."
  (concatenate 'string (string-downcase symbol) "-submit"))

(defun validate-form (form))

(defmacro define-form (form-name fields)
  `(defparameter ,form-name ',fields))

(defun who-args-filter-keys (arguments)
  (destructuring-bind
      (parameter-name &key
                        real-name
                        parameter-type
                        init-form
                        request-type
                        validate)
      arguments
    (remove
     nil
     (concatenate
      'list
      (list parameter-name)
      (when real-name `(:real-name ,real-name))
      (when parameter-type `(:parameter-type ,parameter-type))
      (when init-form `(:init-form ,init-form))
      (when request-type `(:request-type ,request-type))))))

(defmacro define-form-handler (description forms &body body)
  "description is the higher level description from
define-rest-handler, lambda list is a list of forms and fields."
  `(define-rest-handler ,description
       ,(mapcan (lambda (form)
		 (mapcar (lambda (field)
		 	   (destructuring-bind
		 	       (parameter-name &key
		 				 real-name
		 				 parameter-type
		 				 init-form
		 				 request-type
		 				 validate)
		 	       field
		 	     (remove
			      nil
			      (concatenate
			       'list
			       (list parameter-name)
			       (when real-name `(:real-name ,real-name))
			       (when parameter-type `(:parameter-type ,parameter-type))
			       (when init-form `(:init-form ,init-form))
			       (when request-type `(:request-type ,request-type))))))
		 	 (symbol-value form)))
	       forms)
     (let ((*form-errors* (make-hash-table))
	   (*form-data* (make-hash-table)))
       ,@(mapcan (lambda (form)
		 (mapcar (lambda (field)
			   `(setf (gethash ',(car field) *form-data*) ,(car field)))
			 (symbol-value form)))
		forms)
       ,@body)))


(defmacro cond-forms (&rest clauses)
  "contans a list of clauses that match form symbol"
     `(or (cond
       ,@(mapcar (lambda (form)
		   (let ((form-name (compute-real-form-name (car form))))
		     `((hunchentoot:post-parameter ,form-name)
		       ,@(remove nil
				   (labels ((validate-field-list (fields)
					      (destructuring-bind
						  (parameter-name &key
								    real-name
								    parameter-type
								    init-form
								    request-type
							  validate)
						  (car fields)
						(cons `(validate-field ',parameter-name
								       *form-errors* ,@validate)
						      (when (cdr fields) (validate-field-list (cdr fields)))))))
				  (validate-field-list (symbol-value (car form)))))
			 (if (= (hash-table-count *form-errors*) 0)
			     (progn
			       ,@(cdr form)
			       ',(car form))
			     ',(car form)))))
		 clauses))
	  nil))


(def-who-macro field-fragment (name description type &key value error)
  `(:div :class (if ,error "clearfix error" "clearfix")
	(:label ,description)
	(:div :class "input"
	      (:input :type ,type :name ,name
		      :class (if ,error "error")
		      :value ,value)
	      (:span :class "help-inline" (cl-who:str ,error)))))


(def-who-macro form-fragment (form fields &key (action "") (class "form-stacked") buttons)
	       `(:form :id (string-downcase (symbol-name ',form))
		       :action ,action :method "post" :class ,class
		       (if (and (eq ',form *current-form*) (> (hash-table-count *form-errors*) 0))
			   (cl-who:htm
			    (:div :class "alert-message error"
				  (:p "Error detected on the page"))))
		       ,@(mapcar (lambda (field)
				   (let ((field-name (car field))
					 (field-title (second field))
					 (field-type (third field)))
				     `(field-fragment (string-downcase (symbol-name ,field-name))
						      ,field-title
						      ,(string-downcase field-type)
						      :error (when (eq ',form *current-form*)
							       (gethash ,field-name *form-errors*))
						      :value (when (and (eq ',form *current-form*)
									(> (hash-table-count *form-errors*) 0))
							       (gethash ,field-name *form-data*)))))
				 fields)
		       (:div :class "actions"
			     ,@buttons)))
