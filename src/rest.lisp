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

;;;; rest.lisp

(in-package #:planet-git)

;; this module provides macros for rest like handlers

(defparameter *rest-handler-alist* nil
  "An alist of \(URI acceptor-names function) lists defined by
DEFINE-REST-HANDLER.")

(defmacro define-rest-handler (description lambda-list &body body)
  (when (atom description)
    (setf description (list description)))
  (destructuring-bind (name &key uri args (acceptor-names t)
                            (default-parameter-type ''string)
                            (default-request-type :both))
      description
    `(progn
       ,@(when uri
               (list
                (with-rebinding (uri)
                  `(progn
                     (setf *rest-handler-alist*
                           (substitute-if
                            (list ,uri ,acceptor-names ',name)
                            (lambda (list)
                                        (and (or (equal ,uri (first list))
                                                 (eq ',name (third list)))
                                             (or (eq ,acceptor-names t)
                                                 (intersection ,acceptor-names
                                                               (second list)))))
                            *rest-handler-alist*))))))
       (defun ,name (&key ,@(loop for part in lambda-list
                               collect (make-defun-parameter part
                                                             default-parameter-type
                                                             default-request-type)))
         ,(if args
              `(register-groups-bind ,args
                   (,uri (request-uri*))
                 ,@body)
              `(progn ,@body))))))

(defun dispatch-rest-handlers (request)
  "This is a dispatcher which returns the appropriate handler
defined with DEFINE-REST-HANDLER, if there is one."
  (loop for (uri acceptor-names rest-handler) in *rest-handler-alist*
     when (and (or (eq acceptor-names t)
                   (find (acceptor-name *acceptor*) acceptor-names :test #'eq))
               (cond ((stringp uri)
                      (let ((scanner (create-scanner uri)))
                        (scan scanner (script-name request))))
                     (t (funcall uri request))))
     do (return rest-handler)))
