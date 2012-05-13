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

(defvar client-ip "UNKNOWN") ; default value for logging output

(defvar *git-commands* (list "git-receive-pack" "git-upload-pack" "git-upload-archive"))
(defvar *planet-git-url* "http://localhost:8000")

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
    (eval '(ql:quickload 'cl-ppcre))
    (eval '(ql:quickload 'drakma))))

(use-package 'drakma)

(defmacro format-string (control-string &rest format-arguments)
  `(with-output-to-string (stream)
     (format stream ,control-string ,@format-arguments)))


(defmacro log-msg (level message &rest format-args)
  `(with-open-file (stream *log-file* :direction :output :if-exists :append :if-does-not-exist :create)
     (multiple-value-bind
           (second minute hour date month)
         (get-decoded-time)
       (let ((log-message
               (concatenate 'string "~2,'0d ~d ~2,'0d:~2,'0d:~2,'0d ~a ~S:"
                            ,message "~%")))
         (format stream log-message
                 (nth month *month-names*)
                 date
                 hour
                 minute
                 second
                 client-ip ; unhygienic
                 ,level
                 ,@format-args)))))


(defun authorised-p (repository key-id)
  "check if a user has permission to access a repository."
  (let* ((url (format-string "~a/~a/key/~a/" *planet-git-url* repository key-id))
         (return-code (nth-value 1 (http-request url))))
    (log-msg 'debug "Asserting access against ~a got return code ~a" url return-code)
    (eq return-code 204)))

(let* ((command (sb-unix::posix-getenv "SSH_ORIGINAL_COMMAND"))
       (ssh-client (sb-unix::posix-getenv "SSH_CLIENT"))
       (key-id (sb-unix::posix-getenv "KEY_ID"))
       (client-ip (subseq ssh-client 0 (position #\Space ssh-client))))
  (unless (cl-ppcre:register-groups-bind
              (git-command repository)
              ("^([^ ]+) '(.+)'$" command)
            (if (and (find git-command *git-commands* :test #'equal) repository)
                (if (authorised-p repository key-id)
                    (progn
                      (log-msg 'info "Running ~a on repository, ~a" git-command repository)
                      (let ((process (sb-ext:run-program "/usr/bin/git" (list "shell" "-c" command) :input t :output t)))
                        (log-msg (if (> (sb-ext:process-exit-code process) 0)
                                     'error 'info)
                                 "Process exited with status: ~a"
                                 (sb-ext:process-exit-code process)))
                      t) ; return success
                    ;; if key isn't alowed to acces the repository then error and exit
                    (progn
                      (log-msg 'info "Access denied to repository ~a from ip ~a with key ~a"
                               repository client-ip key-id)
                      (print "Access Denied." *error-output*)))
                ;; if the command isn't in the list of allowed cammands
                (progn
                  (log-msg 'error "Invalid command: ~a" command)
                  (print "Invalid Command." *error-output*))))
    ;; if the command doesn't match regular expression then error
    (progn
      (log-msg 'error "Invalid command: ~a" command)
      (print "Invalid Command." *error-output*))))
