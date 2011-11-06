(ql:quickload 'swank)
(ql:quickload 'planet-git)
(require :planet-git)
;; define some parameters for easier update
(defparameter *httpd-port* 8000)     ; The port Hunchentoot will be listening on
(defparameter *shutdown-port* 6200)  ; The port SBCL will be listening for shutdown
                                     ; this port is the same used in /etc/init.d/hunchentoot
(defparameter *swank-port* 4005)     ; The port used for remote interaction with slime

;; Start the Swank server
(defparameter *swank-server*
  (swank:create-server :port *swank-port* :style :spawn :dont-close t))

;;;
;;; The Hunchentoot logic goes in here
;;; this can be just a simple package loading
;;; calling some methods or it might be something else
;;;
;;; The following code is a mere demonstration
;;;
(defparameter *httpd*
  (hunchentoot:start
   (make-instance 'hunchentoot:easy-acceptor
                  :port *httpd-port*)))
(princ ";; Hunchentoot started on port ")
(princ *httpd-port*)
(terpri)

;;; We need a way to actually kill this baby so we
;;; setup a socket listening on a specific port.
;;; When we want to stop the lisp process we simply
;;; telnet to that port as run by the stop section
;;; of the /etc/init.d/hunchentoot script.
;;; This thread will block execution until the
;;; connection comes in on the specified port,
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

;;; The following code won't be reached until a connection
;;; to the shutdown port is made, from here on we clean
;;; everything up and shutdown SBCL.

;;; Since we started a hunchentoot acceptor we should stop it
(print "Stopping Hunchentoot...")
;(hunchentoot:stop *httpd*)

;;; Here we go about closing all the running threads
;;; including the Swank Server we created.
(dolist (thread (sb-thread:list-all-threads))
  (unless (equal sb-thread:*current-thread* thread)
    (sb-thread:terminate-thread thread)))
(sleep 1)
(sb-ext:quit)