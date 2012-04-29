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

;;;; git-shell.lisp
(defvar *debug-log-file* "/tmp/debug-log.txt")
(defvar *log-file* "/tmp/log.txt")

(defvar *client-ip* "UNKNOWN")

(defvar *month-names* '("Zero" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
	"Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(with-open-file (stream *debug-log-file* :direction :output :if-exists :append :if-does-not-exist :create)
  (let ((*standard-output* stream)
	(*error-output* stream))
    (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
					   (user-homedir-pathname))))
      (when (probe-file quicklisp-init)
	(load quicklisp-init)))))

(with-open-file (stream *debug-log-file* :direction :output :if-exists :append :if-does-not-exist :create)
  (let ((*standard-output* stream)
	(*error-output* stream))
    (eval '(ql:quickload 'cl-ppcre))))

(defmacro log-msg (message &rest format-args)
  `(with-open-file (stream *log-file* :direction :output :if-exists :append :if-does-not-exist :create)
    (multiple-value-bind
	  (second minute hour date month)
	(get-decoded-time)
      (let ((log-message
	      (concatenate 'string "~2,'0d ~d ~2,'0d:~2,'0d:~2,'0d ~a "
			   ,message "~%")))
	(format stream log-message
	      (nth month *month-names*)
	      date
	      hour
	      minute
	      second
	      *client-ip*
	      ,@format-args)))))

(let* ((command (sb-unix::posix-getenv "SSH_ORIGINAL_COMMAND"))
       (ssh-client (sb-unix::posix-getenv "SSH_CLIENT"))
       (*client-ip* (subseq ssh-client 0 (position #\Space ssh-client))))
  (unless (cl-ppcre:register-groups-bind
	      (git-command repository)
	      ("^([^ ]+) '(.+)'$" command)
	    (if (and (find git-command
			   (list "git-receive-pack" "git-upload-pack" "git-upload-archive")
			   :test #'equal)
		     repository)
		(progn
		  (log-msg "INFO: Running ~a on repository, ~a" git-command repository)
		  (let ((process (sb-ext:run-program "/usr/bin/git" (list "shell" "-c" command) :input t :output t)))
		    (if (> (sb-ext:process-exit-code process) 0)
			(log-msg "ERROR: Process exited with status: ~a"
				 (sb-ext:process-exit-code process))
			(log-msg "INFO: Process exited with status: ~a"
				 (sb-ext:process-exit-code process))))
		  t) ; return success
		(progn
		  (log-msg "ERROR: invalid command: ~a" command)
		  (print "ERROR: Invalid Command." *error-output*))))
    (progn
      (log-msg "ERROR: invalid command: ~a" command)
      (print "ERROR: Invalid Command." *error-output*))))
