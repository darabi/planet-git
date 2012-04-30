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

;;;; home.lisp

(in-package #:planet-git)


(def-who-macro user-item-fragment (user)
  "Create a users description for the front page"
  `(cl-who:htm
    (:div :class "well user"
	  (:a :href (cl-who:str (url-join (slot-value ,user 'username)))
          (:img :src (gravatar-url
                       (slot-value user 'email)
                       :size 40))
	      (:h3 :class "name"
		   (cl-who:str (slot-value ,user 'username)))))))


(defun home-page ()
  (render-standard-page (:title "Planet Git"
                                :subtitle "a bad clone of github or gitorious.")
	    (let ((users (postmodern:select-dao 'login)))
          (loop :for user :in users
               :do (user-item-fragment user)))))
