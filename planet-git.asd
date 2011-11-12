;;;; planet-git.asd

(asdf:defsystem #:planet-git
  :serial t
  :depends-on (#:hunchentoot
               #:cl-who
               #:cl-git
               #:postmodern
	       #:cl-ppcre
	       #:cl-fad)
  :components ((:file "package")
               (:file "planet-git")))

