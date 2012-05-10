#!/usr/bin/sbcl --script
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

;;;; develop.lisp

;;; Reenable the debugger
(sb-ext:enable-debugger)

(load "~/.sbclrc")
(use-package 'asdf)
(asdf:oos 'asdf:load-op :quicklisp)

(ql:quickload 'swank)
(push (probe-file #p"./") asdf:*central-registry*)

(ql:quickload 'planet-git)
(use-package 'unix-options)


;; define some parameters for easier update
(defparameter *config* (py-configparser:make-config))

;; set some default settings
(flet ((set-option (section option value)
         (py-configparser:set-option *config* section option value))
       (add-section (section)
         (py-configparser:add-section *config* section)))

  ;; default swank configuration
  (add-section "swank")
  (set-option "swank" "enabled" "no")
  (set-option "swank" "port" "4005")

  ;; default webserver configuration
  (add-section "webserver")
  (set-option "webserver" "port" "8000")
  (set-option "webserver" "shutdown-port" "6200")

  ;; default database configuration
  (add-section "database")
  (set-option "database" "database" "planet_git")
  (set-option "database" "host" "localhost")
  (set-option "database" "port" "5432")
  (set-option "database" "username" "gitui")
  (set-option "database" "password" "oenRTe90u")

  ;; default planet-git configuration
  (add-section "planet-git")
  (set-option "planet-git" "repository-path" "repository-path")
  (set-option "planet-git" "git-ssh-host" "git@localhost"))

(with-cli-options ()
    (help &parameters config)
  (when help
    (print-usage-summary "Usage:~%~@{~A~%~}"
                         '(((#\c "config") "FILENAME" "path to the config file")))
    (quit :unix-status 1))
  (py-configparser:read-files
   *config*
   (list (pathname config))))

(flet ((get-option (option)
         (parse-integer
          (py-configparser:get-option *config* "swank" option))))
  (defparameter *swank-port* (get-option "port")))
(defparameter *swank-server* nil)

(defparameter *trueish* (list "yes" "enable" "y" "on" "true"))
(defun option-in-list (config section option list)
  "return the value of the OPTION from the SECTION if it's value is in
the LIST"
  (let ((opt (py-configparser:get-option config section option)))
    (flet ((match-list (item)
             (equal item opt)))
      (car (remove-if-not #'match-list list)))))

;; Enable SWANK if it's specified in the config file
(if (option-in-list *config* "swank" "enabled" *trueish*)
  (setf *swank-server*
        (swank:create-server :port *swank-port* :style :spawn :dont-close t)))

;;;
;;; Configure the database
;;;

(flet ((get-option (option)
          (coerce (py-configparser:get-option *config* "database" option)
                  'simple-string)))
  (postmodern:connect-toplevel (get-option "database")
                               (get-option "username")
                               (get-option "password")
                               (get-option "host")
                               :port (parse-integer (get-option "port"))))

(planet-git:create-tables)

;;;
;;; Configure Planet-Git
;;;

(flet ((get-option (option)
          (py-configparser:get-option *config* "planet-git" option)))
  (setq planet-git:*repository-directory* (pathname (get-option "repository-path")))
  (setq planet-git:*git-ssh-host* (pathname (get-option "git-ssh-host"))))


;;;
;;; Load Hunchentoot
;;;
(flet ((get-option (option)
         (parse-integer
          (py-configparser:get-option *config* "webserver" option))))
  (defparameter *httpd-port* (get-option "port"))
  (defparameter *shutdown-port* (get-option "shutdown-port")))

(defparameter *httpd*
  (hunchentoot:start
   (make-instance 'hunchentoot:easy-acceptor
                  :port *httpd-port*)))
(format t ";; Hunchentoot started at port: ~s.~%" *httpd-port*)

(defun escape-orbit ()
  (dolist (thread (sb-thread:list-all-threads))
    (unless (equal sb-thread:*current-thread* thread)
      (sb-thread:terminate-thread thread))))

;;; This thread will block execution until the connection comes in on
;;; the specified port,
(format t ";; Shutdown service started at port: ~s.~%" *shutdown-port*)
(let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                             :type :stream :protocol :tcp)))

  ;; Listen on a local port for a TCP connection
  (sb-bsd-sockets:socket-bind socket #(127 0 0 1) *shutdown-port*)
  (sb-bsd-sockets:socket-listen socket 1)

  ;; When it comes, close the sockets and continue
  (multiple-value-bind (client-socket addr port)
      (sb-bsd-sockets:socket-accept socket)
    (sb-bsd-sockets:socket-close client-socket)
    (sb-bsd-sockets:socket-close socket)))

;;; Since we started a hunchentoot acceptor we should stop it
(print "Stopping Hunchentoot...")
(hunchentoot:stop *httpd*)

(escape-orbit)
(sleep 1)
(sb-ext:quit)
