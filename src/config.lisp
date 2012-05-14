;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

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

;; The configuration code was taken from elephant, the lisp object
;; database

(in-package #:planet-git)

(defparameter *swank-server* nil
  "Bound to the running swank server, if *SWANK-ENABLED* is true.")

(defparameter *httpd* nil
  "Bound to hunchentoot instance after startup.")

(defparameter *swank-enabled* nil
  "If set to true, a swank server is started on port *SWANK-PORT*")

(defparameter *swank-port* 4005
  "If *SWANK-ENABLED* is true, a swank server is started on this port.")

(defparameter *webserver-port* 8000
  "The hunchentoot server listens on this port.")

(defparameter *shutdown-port* 6200
  "Opening a TCP connection from localhost to this port shuts down
  planet-git without further checks.")

(defparameter *db-host* "localhost"
  "The host name or IP of the PostgreSQL instance which provides the
  database.")

(defparameter *db-port* 5432
  "Database port")

(defparameter *db-name* "planet_git"
  "Database schema name.")

(defparameter *db-user-name* "gitui"
  "Database user name")

(defparameter *db-password* "oenRTe90u"
  "Database user name")

(defparameter *repository-path* "/home/russell/tmp/planet-git/"
  "The location where new git repositories are created.")

(defparameter *git-ssh-host* "git@marvin.home"
  "Host which is used with Git SSH (can include the user name
  e.g. 'user@my.git.host'")

;; TODO: create a macro to avoid carpal tunnel syndrome from typing a
;; parameter name thrice
(defvar *user-configurable-parameters*
  '((:swank-enabled *swank-enabled*)
    (:swank-port *swank-port*)
    (:webserver-port *webserver-port*)
    (:shutdown-port *shutdown-port*)
    (:db-host *db-host*)
    (:db-port *db-port*)
    (:db-name *db-name*)
    (:db-user-name *db-user-name*)
    (:db-password *db-password*)
    (:repository-path *repository-path*)
    (:git-ssh-host *git-ssh-host*)))

(defun initialize-user-parameters ()
  (loop for (keyword variable) in *user-configurable-parameters* do
       (awhen (get-user-configuration-parameter keyword)
	 (setf (symbol-value variable) it))))

(defun get-config-option (option component)
  (let ((filespec (make-pathname :defaults (asdf:component-pathname (asdf:component-system component))
				 :name "config"
				 :type "sexp"))
	(orig-filespec (make-pathname :defaults (asdf:component-pathname (asdf:component-system component))
				 :name "config"
				 :type "sexp.in")))
    (unless (probe-file filespec)
      (with-simple-restart (accept-default "Create default settings for config.sexp and proceed.")
	(error "Missing configuration file: config.sexp.  Please copy config.sexp.in to config.sexp and customize for your local environment."))
      (with-open-file (src orig-filespec :direction :input)
	(with-open-file (dest filespec :direction :output)
	  (write (read src) :stream dest))))
    (with-open-file (config filespec)
      (cdr (assoc option (read config))))))

(defun get-user-configuration-parameter (name)
  "This function pulls a value from the key-value pairs stored in
   my-config.sexp so data stores can have their own pairs for appropriate
   customization after loading."
  (get-config-option name (asdf:find-system :planet-git)))

(defun wait-for-shutdown-connection (&optional (port *shutdown-port*))
  "Opens a server socket at the given port (default *SHUTDOWN-PORT*,
waits for a connection indefinitely."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    ;; Listen on the given port for a TCP connection
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) port)
    (sb-bsd-sockets:socket-listen socket 1)
    ;; When it comes, close the sockets and continue
    (let ((client-socket (sb-bsd-sockets:socket-accept socket)))
      (sb-bsd-sockets:socket-close client-socket)
      (sb-bsd-sockets:socket-close socket))))

(defun startup ()
  (initialize-user-parameters)
  (setf *swank-server* (when *swank-enabled*
                         (swank:create-server :port *swank-port* :style :spawn :dont-close t)))
  (connect-toplevel *db-name*
                               *db-user-name*
                               *db-password*
                               *db-host*
                               :port *db-port*)
  (create-tables)
  (setf *httpd* (start (make-instance 'acceptor
                                                  :port *webserver-port*)))
  (format t ";; Hunchentoot started at port: ~s.~%" *webserver-port*)
  *httpd*)

(defun shutdown ()
  (format t ";; Disconnecting from planet-git database ... ")
  (disconnect *database*)
  (format t "done.~%")
  (format t ";; Stopping Hunchentoot ... ")
  (stop *httpd*)
  (format t "done.~%"))

(defun main ()
  (startup)
  (format t ";; Starting shutdown service at port: ~s.~%" *shutdown-port*)
  (wait-for-shutdown-connection)
  (format t ";; Shutdown connection detected at port ~S. Attempting to stop ... " *shutdown-port*)
  (shutdown)
  (format t "done.~%") )
