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

;;;; utils.lisp

(in-package #:planet-git)


(defun assoc-default (item alist &key key test test-not)
  "return the CDR of the first cons in alist whose car satisfies the
test, or nil if no such cons is found."
  (if test
      (cdr (assoc item alist)); :key key :test test))
      (cdr (assoc item alist)))); :key key :test-not test-not))))
