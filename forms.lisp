;;;; forms.lisp

(in-package #:planet-git)

(defparameter *form-errors* (make-hash-table))

(defun compute-real-form-name (symbol)
  "Computes the `real' paramater name \(a string) from the Lisp symbol
SYMBOL.  Used in cases where no parameter name is provided."
  (concatenate 'string (string-downcase symbol) "-submit"))

(defun validate-form (form))

(defmacro define-form (form-name fields)
  `(defparameter ,form-name ',fields))

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
     (let ((*form-errors* (make-hash-table)))
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
			 (when (= (hash-table-count *form-errors*) 0)
			     (progn ,@(cdr form))))))
		 clauses))))
