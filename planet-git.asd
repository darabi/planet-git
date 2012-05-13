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

;;;; planet-git.asd

(asdf:defsystem #:planet-git
  :serial t
  :depends-on (#:anaphora
               #:hunchentoot
               #:chunga
               #:cl-who
               #:cl-git
               #:postmodern
               #:cl-ppcre
               #:css-lite
               #:database-migrations
               #:parenscript
               #:unix-options
               #:md5)
  :components ((:static-file "planet-git.asd")
               (:file "package")
               (:file "config")
               (:file "compat")
               (:file "utils")
               (:file "easy-handlers")
               (:file "rest")
               (:file "templates")
               (:file "forms")
               (:file "validators")
               (:module "views" :depends-on ("forms" "validators" "templates")
                        :components
                        ((:file "home")
                         (:file "user")
                         (:file "registration")
                         (:file "repository")))
               (:file "planet-git" :depends-on ("views"))))
