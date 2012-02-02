;;;; rest.lisp

(in-package #:planet-git)

;; this module provides macros for rest like handlers

(defvar *rest-handler-alist* nil
  "An alist of \(URI acceptor-names function) lists defined by
DEFINE-EASY-HANDLER.")

(defun compute-real-name (symbol)
  "Computes the `real' paramater name \(a string) from the Lisp
symbol SYMBOL.  Used in cases where no parameter name is
provided."
  ;; we just downcase the symbol's name
  (string-downcase symbol))

(defun convert-parameter (argument type)
  "Converts the string ARGUMENT to TYPE where TYPE is one of the
symbols STRING, CHARACTERS, INTEGER, KEYWORD, or BOOLEAN - or
otherwise a function designator for a function of one argument.
ARGUMENT can also be NIL in which case this function also returns
NIL unconditionally."
  (when (listp argument)
    ;; this if for the case that ARGUMENT is NIL or the result of a
    ;; file upload
    (return-from convert-parameter argument))
  (case type
    (string argument)
    (character (and (= (length argument) 1)
                    (char argument 0)))
    (integer (ignore-errors* (parse-integer argument :junk-allowed t)))
    (keyword (as-keyword argument :destructivep nil))
    (boolean t)
    (otherwise (funcall type argument))))

(defun compute-simple-parameter (parameter-name type parameter-reader)
  "Retrieves the parameter named PARAMETER-NAME using the reader
PARAMETER-READER and converts it to TYPE."
  (convert-parameter (funcall parameter-reader parameter-name) type))

(defun compute-list-parameter (parameter-name type parameters)
  "Retrieves all parameters from PARAMETERS which are named
PARAMETER-NAME, converts them to TYPE, and returns a list of
them."
  (loop for (name . value) in parameters
        when (string= name parameter-name)
        collect (convert-parameter value type)))

(defun compute-array-parameter (parameter-name type parameters)
  "Retrieves all parameters from PARAMETERS which are named like
\"PARAMETER-NAME[N]\" \(where N is a non-negative integer),
converts them to TYPE, and returns an array where the Nth element
is the corresponding value."
  ;; see <http://common-lisp.net/pipermail/tbnl-devel/2006-September/000660.html>
  #+:sbcl (declare (sb-ext:muffle-conditions warning))
  (let* ((index-value-list
          (loop for (full-name . value) in parameters
                for index = (cl-ppcre:register-groups-bind (name index-string)
                                ("^(.*)\\[(\\d+)\\]$" full-name)
                              (when (string= name parameter-name)
                                (parse-integer index-string)))
                when index
                collect (cons index (convert-parameter value type))))
         (array (make-array (1+ (reduce #'max index-value-list
                                        :key #'car
                                        :initial-value -1))
                            :initial-element nil)))
    (loop for (index . value) in index-value-list
          do (setf (aref array index) value))
    array))

(defun compute-hash-table-parameter (parameter-name type parameters key-type test-function)
  "Retrieves all parameters from PARAMETERS which are named like
\"PARAMETER-NAME{FOO}\" \(where FOO is any sequence of characters
not containing curly brackets), converts them to TYPE, and
returns a hash table with test function TEST-FUNCTION where the
corresponding value is associated with the key FOO \(converted to
KEY-TYPE)."
  (let ((hash-table (make-hash-table :test test-function)))
    (loop for (full-name . value) in parameters
          for key = (cl-ppcre:register-groups-bind (name key-string)
                        ("^(.*){([^{}]+)}$" full-name)
                      (when (string= name parameter-name)
                        (convert-parameter key-string key-type)))
          when key
          do (setf (gethash key hash-table)
                   (convert-parameter value type)))
    hash-table))

(defun compute-parameter (parameter-name parameter-type request-type)
  "Computes and returns the parameter\(s) called PARAMETER-NAME
and converts it/them according to the value of PARAMETER-TYPE.
REQUEST-TYPE is one of :GET, :POST, or :BOTH."
  (when (member parameter-type '(list array hash-table))
    (setq parameter-type (list parameter-type 'string)))
  (let ((parameter-reader (ecase request-type
                              (:get #'hunchentoot:get-parameter)
                              (:post #'hunchentoot:post-parameter)
                              (:both #'hunchentoot:parameter)))
        (parameters (and (listp parameter-type)
                         (case request-type
                           (:get (hunchentoot:get-parameters*))
                           (:post (hunchentoot:post-parameters*))
                           (:both (append (hunchentoot:get-parameters*)
					  (hunchentoot:post-parameters*)))))))
    (cond ((atom parameter-type)
           (compute-simple-parameter parameter-name parameter-type parameter-reader))
          ((and (null (cddr parameter-type))
                (eq (first parameter-type) 'list))
           (compute-list-parameter parameter-name (second parameter-type) parameters))
          ((and (null (cddr parameter-type))
                (eq (first parameter-type) 'array))
           (compute-array-parameter parameter-name (second parameter-type) parameters))
          ((and (null (cddddr parameter-type))
                (eq (first parameter-type) 'hash-table))
           (compute-hash-table-parameter parameter-name (second parameter-type) parameters
                                         (or (third parameter-type) 'string)
                                         (or (fourth parameter-type) 'equal)))
          (t (hunchentoot:parameter-error "Don't know what to do with parameter type ~S."
					  parameter-type)))))

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
    `(,parameter-name (or (and (boundp 'hunchentoot:*request*)
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
                 (setq *rest-handler-alist*
                       (delete-if (lambda (list)
                                    (and (or (equal ,uri (first list))
                                             (eq ',name (third list)))
                                         (or (eq ,acceptor-names t)
                                             (intersection ,acceptor-names
                                                           (second list)))))
                                  *rest-handler-alist*))
                 (push (list ,uri ,acceptor-names ',name) *rest-handler-alist*)))))
       (defun ,name (&key ,@(loop for part in lambda-list
                                  collect (make-defun-parameter part
                                                                default-parameter-type
                                                                default-request-type)))
	 ,(if args
	      `(cl-ppcre:register-groups-bind ,args
		  (,uri (hunchentoot:request-uri*))
		,@body)
	      `(,@body))))))

(defun dispatch-rest-handlers (request)
  "This is a dispatcher which returns the appropriate handler
defined with DEFINE-REST-HANDLER, if there is one."
  (loop for (uri acceptor-names rest-handler) in *rest-handler-alist*
        when (and (or (eq acceptor-names t)
                      (find (hunchentoot:acceptor-name hunchentoot:*acceptor*) acceptor-names :test #'eq))
                  (cond ((stringp uri)
			 (let ((scanner (cl-ppcre:create-scanner uri)))
			   (cl-ppcre:scan scanner (hunchentoot:script-name request))))
                        (t (funcall uri request))))
        do (return rest-handler)))
