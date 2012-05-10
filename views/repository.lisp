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

;;;; repository.lisp

(in-package #:planet-git)


(define-rest-handler (repository-home-page
                      :uri "^/([^/]+)/([^/]+)/?$"
                      :args (username repository-name))
    ()
    (repository-page username repository-name))


(define-rest-handler (repository-branch-page
                      :uri "^/([^/]+)/([^/]+)/branch/([^/]+)/?$"
                      :args (username repository-name branch))
    ()
  (let
      ((ref (concatenate 'string "refs/heads/" branch)))
    (repository-page username repository-name :branch ref)))


(hunchentoot:define-easy-handler
    (new-repository-page :uri "/repository/new")
    ((name :parameter-type 'string)
     (public :parameter-type 'boolean))
  (let* ((errors (validate-newrepository)))
    (if (and (= (hash-table-count errors) 0)
	     (eq (hunchentoot:request-method*) :post))
	(progn
	  (create-repository name (loginp) public)
	  (hunchentoot:redirect (concatenate 'string "/"
					     (slot-value (loginp) 'username) "/"name "/")))
      (render-standard-page (:title "New Repository")
	(:form :action "" :method "post" :class "form-stacked"
	       (if (> (hash-table-count errors) 0)
		   (cl-who:htm
		    (:div :class "alert-message error"
			  (:p "Error detected on the page"))))
	       (field-fragment "name" "Name:" "text"
		      :error (gethash 'name errors))
	       (field-fragment "public" "Public:" "checkbox"
		      :error (gethash 'public errors))
	       (:div :class "actions"
		     (:input :type "submit"
			     :class "btn primary"
			     :name "submit"
			     :value "Create")))))))



(defun repository-page (username repository-name &key branch)
  (let*
      ((user (car (postmodern:select-dao 'login (:= 'username username))))
       (repository (car (postmodern:select-dao
			 'repository (:and
				      (:= 'owner-id (slot-value user 'id))
				      (:= 'name repository-name)))))
       (visible (when repository (or (slot-value repository 'public)
				     (equal (slot-value user 'username)
					    (when (loginp) (slot-value (loginp) 'username))))))
       (is-current-user (when user (equal (slot-value user 'username)
					  (when (loginp) (slot-value (loginp) 'username))))))
    (if (and visible user repository)
	(cl-git:with-git-repository ((repository-real-path repository))
	  (let* ((branches (cl-git:git-reference-listall))
		 (branch (selected-branch repository branches branch)))
	    (render-user-page (user :title
				   (cl-who:htm (:a :href (url-join (slot-value user 'username))
						   (cl-who:str (user-username user)))
					       (:span (cl-who:str "/"))
					       (cl-who:str (repository-name repository)))
                   :subtitle "")
	      (cond
		(branch
		 (cl-who:htm
		  (:script :type "text/javascript"
			   (cl-who:str
			    (ps:ps
			      (defun select-branch (branch)
				(setf (ps:getprop window 'location 'href)
				      (concatenate 'string
						   (ps:lisp (url-join username repository-name "branch"))
						   branch "/"))))))
		  (:div :class "project-bar"
			(:select :id "branch"
				 :onchange (ps:ps-inline (select-branch
							  (ps:@ this options
								(ps:@ this selected-index) value)))
				 (mapcar #'(lambda (x)
					     (cl-who:htm
					      (:option
					       :value (remove-ref-path x)
					       :selected (when (equal x branch) "true")
					       (cl-who:str (remove-ref-path x)))))
					 (cl-git:git-reference-listall))))
		  (:ol :class "commit-list"
		       (let ((count 0))
			 (cl-git:with-git-revisions (commit :head branch)
			   (setf count (+ count 1))
			   (when (> count 10) (return))
			   (cl-who:htm
			    (:li
			     (let* ((author (cl-git:git-commit-author commit))
				    (name (first author))
				    (email (second author))
				    (timestamp (third author)))
			       (cl-who:htm
				(:img :src (gravatar-url email :size 40))
				(:p (cl-who:str (cl-git:git-commit-message commit)))
				(:span :class "author" (cl-who:str name))
				(:span :class "date"
				       (cl-who:str
                        (local-time:format-timestring nil timestamp :format
                                                      '(:long-month " " :day ", " :year))))))
			     )))))))
		((and (eq branch nil) is-current-user)
		 (cl-who:htm
		  (:div :class "well"
			(:h2 "Welcome to your new repository."))
		  ))
		((eq branch nil)
		 (cl-who:htm
		  (:div :class "well"
			(:h2 "Under Construction."))
		  ))
		(t (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))))
