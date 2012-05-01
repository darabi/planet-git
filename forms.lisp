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

;;;; forms.lisp

(in-package #:planet-git)

(defparameter *forms* nil)
(defparameter *form-errors* (make-hash-table))
(defparameter *form-data* (make-hash-table))
(defparameter *current-form* nil)


(defun compute-real-form-name (symbol)
  "Computes the `real' paramater name \(a string) from the Lisp symbol
SYMBOL.  Used in cases where no parameter name is provided."
  (concatenate 'string (string-downcase symbol) "-submit"))

(defun compute-real-field-name (form field)
  "Computes the `real' paramater name \(a string) from the Lisp symbol
SYMBOL.  Used in cases where no parameter name is provided."
  (concatenate 'string (string-downcase form) "-" (string-downcase field)))

(defun compute-field-symbol (form field)
  "Computes the `real' paramater name \(a string) from the Lisp symbol
SYMBOL.  Used in cases where no parameter name is provided."
  (intern (concatenate 'string (symbol-name form) "-" (symbol-name field))))

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
       ,(remove-duplicates ; remove duplicate fields, should be
                                        ; changed to a more advanced function that
                                        ; can merge field types like post and get.
         (mapcan (lambda (form)
                   (mapcar (lambda (field)
                             (destructuring-bind
                                   (parameter-name &key
                                                   real-name
                                                   parameter-type
                                                   init-form
                                                   request-type
                                                   &allow-other-keys)
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
                           (cdr form)))
                 forms) :test (lambda (a b) (eq (car a) (car b))))
     (let ((*form-errors* (make-hash-table))
           (*forms* (quote ,forms))
           (*form-data* (make-hash-table)))
       ,@(mapcan (lambda (form)
                   (mapcar (lambda (field)
                             `(setf (gethash ',(car field) *form-data*)
                                    ,(car field)))
                           (cdr form)))
                 forms)
       ,@body)))


(defun validate-field-list (fields)
  "Validate a list of FIELDS as a side-effect add any errors that are
found to the global variable *FIELD-ERRORS*."
  (mapcar #'eval
          (remove
           nil
           (labels ((%validate-field-list (fields)
                      (destructuring-bind
                            (parameter-name &key validate &allow-other-keys)
                          (car fields)
                        (cons `(validate-field ',parameter-name
                                               *form-errors* ,@validate)
                              (when (cdr fields) (%validate-field-list (cdr fields)))))))
             (%validate-field-list fields)))))


(defmacro cond-forms (&rest clauses)
  "contans a list of clauses that match form symbol"
  `(or
    (cond
      ,@(mapcar
         (lambda (form)
           (let ((form-name (compute-real-form-name (car form))))
             `((hunchentoot:post-parameter ,form-name)
               (validate-field-list (assoc-default (quote ,(car form)) *forms*))
               (if (= (hash-table-count *form-errors*) 0)
                   (progn
                     (hunchentoot:log-message*
                      hunchentoot:*lisp-warnings-log-level*
                      "Form submitted: ~s" ,form-name)
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


(def-who-macro form-fragment
    (form fields &key (action "") (class "form-stacked") buttons)
  `(:form :id (string-downcase (symbol-name ',form))
          :action ,action :method "post" :class ,class
          (if (and (eq ',form *current-form*)
                   (> (hash-table-count *form-errors*) 0))
              (cl-who:htm
               (:div :class "alert-message error"
                     (:p "Error detected on the page"))))
          ,@(mapcar
             (lambda (field)
               (destructuring-bind
                     (field-name field-title field-type &key value error)
                   field
                 `(field-fragment (string-downcase (symbol-name ,field-name))
                                  ,field-title
                                  ,(string-downcase field-type)
                                  :error (when (eq ',form *current-form*)
                                           (or (gethash ,field-name *form-errors*)
                                               ,error))
                                  :value (if (eq ',form *current-form*)
                                             (gethash ,field-name *form-data*)
                                             ,value))))
             fields)
          (:div :class "actions"
                ,@buttons)))
