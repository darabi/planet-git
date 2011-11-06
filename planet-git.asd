;;;; planet-git.asd

(asdf:defsystem #:planet-git
  :serial t
  :depends-on (#:hunchentoot
               #:cl-who
               #:postmodern)
  :components ((:file "package")
               (:file "planet-git")))

