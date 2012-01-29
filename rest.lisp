;; this module provides macros for rest like handlers

(defun compute-real-name (symbol)
  "Computes the `real' paramater name \(a string) from the Lisp
symbol SYMBOL.  Used in cases where no parameter name is
provided."
  ;; we just downcase the symbol's name
  (string-downcase symbol))

(defun make-defun-parameter (description default-parameter-type default-request-type)
  "Creates a keyword parameter to be used by DEFINE-EASY-HANDLER.
DESCRIPTION is one of the elements of DEFINE-EASY-HANDLER's
LAMBDA-LIST and DEFAULT-PARAMETER-nTYPE and DEFAULT-REQUEST-TYPE
are the global default values."
  (when (atom description)
    (setq description (list description)))
  (destructuring-bind (parameter-name &key (real-name (compute-real-name parameter-name))
                                           parameter-type init-form request-type)
      description
    `(,parameter-name (or (and (boundp '*request*)
                               (compute-parameter ,real-name
                                                  ,(or parameter-type default-parameter-type)
                                                  ,(or request-type default-request-type)))
                          ,init-form))))


(defmacro define-rest-handler (description lambda-list &body body)
  (when (atom description)
    (setq description (list description)))
  (destructuring-bind (name &key uri args (acceptor-names t)
                            (default-parameter-type ''string)
                            (default-request-type :both))
      description
    `(progn
       ,@(when uri
           (list
            (with-rebinding (uri)
              `(progn
                 (setq *easy-handler-alist*
                       (delete-if (lambda (list)
                                    (and (or (equal ,uri (first list))
                                             (eq ',name (third list)))
                                         (or (eq ,acceptor-names t)
                                             (intersection ,acceptor-names
                                                           (second list)))))
                                  *easy-handler-alist*))
                 (push (list ,uri ,acceptor-names ',name) *easy-handler-alist*)))))
       (defun ,name (&key ,@(loop for part in lambda-list
                                  collect (make-defun-parameter part
                                                                default-parameter-type
                                                                default-request-type)))
	 ,(if args
	      `(cl-ppcre:register-groups-bind ,args
		  (,uri (hunchentoot:request-uri*))
		,@body)
	      `(,@body))))))
