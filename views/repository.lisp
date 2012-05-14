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


(define-rest-handler (repository-key-access
                      :uri "^/([^/]+)/([^/]+)/key/([^/]+)/?$"
                      :args (username repository-name key-id))
    ()
  (let ((key-id (parse-integer key-id)))
    (if
     (let ((owner-key (query (:select '*
                       :from 'key
                       :inner-join 'login :on (:= 'key.user-id 'login.id)
                       :inner-join 'repository :on (:= 'login.id 'repository.owner-id)
                       :where (:and (:= 'key.id key-id)
                                    (:= 'repository.name repository-name)
                                    (:= 'login.username username))))))
       owner-key)

     (setf (return-code*) +http-no-content+)
     (setf (return-code*) +http-forbidden+)))
  "") ;; return an empty string for the content


(define-easy-handler
    (new-repository-page :uri "/repository/new")
    ((name :parameter-type 'string)
     (public :parameter-type 'boolean))
  (let* ((errors (validate-newrepository)))
    (if (and (= (hash-table-count errors) 0)
	     (eq (request-method*) :post))
	(progn
	  (create-repository name (loginp) public)
	  (redirect (concatenate 'string "/"
					     (slot-value (loginp) 'username) "/"name "/")))
      (render-standard-page (:title "New Repository")
	(:form :action "" :method "post"
	       (if (> (hash-table-count errors) 0)
		   (htm
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
      ((user (car (select-dao 'login (:= 'username username))))
       (repository (car (select-dao
			 'repository (:and
				      (:= 'owner-id (slot-value user 'id))
				      (:= 'name repository-name)))))
       (visible (when repository (or (slot-value repository 'public)
				     (equal (slot-value user 'username)
					    (when (loginp) (slot-value (loginp) 'username))))))
       (is-current-user (when user (equal (slot-value user 'username)
					  (when (loginp) (slot-value (loginp) 'username))))))
    (if (and visible user repository)
	(with-git-repository ((repository-real-path repository))
	  (let* ((branches (git-reference-listall))
		 (branch (selected-branch repository branches branch)))
	    (render-user-page (user :title
				   (htm (:a :href (url-join (slot-value user 'username))
						   (str (user-username user)))
					       (:span (str "/"))
					       (str (repository-name repository)))
                   :subtitle "")
	      (cond
		(branch
		 (htm
		  (:script :type "text/javascript"
			   (str
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
					     (htm
					      (:option
					       :value (remove-ref-path x)
					       :selected (when (equal x branch) "true")
					       (str (remove-ref-path x)))))
					 (git-reference-listall))))
		  (:ol :class "commit-list"
		       (let ((count 0))
			 (with-git-revisions (commit :head branch)
			   (setf count (+ count 1))
			   (when (> count 10) (return))
			   (htm
			    (:li
			     (let* ((author (git-commit-author commit))
				    (name (first author))
				    (email (second author))
				    (timestamp (third author)))
			       (htm
				(:img :src (gravatar-url email :size 40))
				(:p (str (git-commit-message commit)))
				(:span :class "author" (str name))
				(:span :class "date"
				       (str
                        (local-time:format-timestring nil timestamp :format
                                                      '(:long-month " " :day ", " :year))))))
			     )))))))
		((and (eq branch nil) is-current-user)
		 (htm
		  (:div :class "well"
			(:h2 "Welcome to your new repository.")
            (:p "First things first, if you haven't already done so,
            you should set up your user preferences."
                (:pre (format t "git config --global user.name \"~A\"
git config --global user.email \"~A\"" (user-fullname user) (user-primary-email user))))
            (:p "If you are adding an existing repository begin by,"
                (:pre (format t "cd existing_repository
git remote add origin ~A:~A/~A
git push origin master" *git-ssh-host* (user-username user) (repository-name repository)))))))
		((eq branch nil)
		 (htm
		  (:div :class "well"
			(:h2 "Under Construction."))
		  ))
		(t (setf (return-code*) +http-not-found+))))))
	(setf (return-code*) +http-not-found+))))
