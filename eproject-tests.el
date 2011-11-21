;;; eproject-tests.el --- unit tests for eproject

;; Copyright (C) 2010  Jonathan Rockway

;; Author: Jonathan Rockway <jon@jrock.us>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is for developers, not end-users.  Run the tests to see
;; whether or not your changes horribly broke eproject.  If they did,
;; fix the code or tests :)

(defvar eproject-tests nil "List of unit tests.")

(defun eproject-test-named (name value)
  "Return T if the test VALUE is named NAME."
  (eq (getf value :test-name) name))

(defmacro define-eproject-test (name &optional docstring &rest body)
  "Define an eproject unit test called NAME that is executed as BODY.
If BODY returns T, the test passes.  If BODY returns NIL, the test fails.
Optional argument DOCSTRING is ... the docstring."
  (declare (debug (&define name [&optional stringp] [&rest sexp] def-body))
	   (doc-string 2))

  `(setq eproject-tests
       (cons
        (list :test-name ',name
              :test-description ,docstring
              :test-body (lambda () ,@body))
        (remove* ',name eproject-tests :test 'eproject-test-named))))

(defun eproject-runtests ()
  (interactive)
  (with-current-buffer (get-buffer-create "*eproject tests*")
    (pop-to-buffer (current-buffer))
    (delete-region (point-min) (point-max))
    (goto-char (point-max))
    (dolist (test (reverse eproject-tests))
      (let* ((name (getf test :test-name))
             (description (getf test :test-description ""))
             (body (getf test :test-body)))
        (if (ignore-errors (funcall body))
            (insert (format "ok '%s' - %s\n" name description))
          (insert (format "not ok '%s' - %s\n" name description)))))))

(define-eproject-test harness-sanity
  "Make sure the test harness sort of works."
  t)

(define-eproject-test eproject-loaded
  "Make sure eproject is on for this file, and that the root is correct."
  (with-current-buffer (get-buffer "eproject-tests.el")
    (equal (eproject-root) (file-name-directory buffer-file-name))))

;;; eproject-tests.el ends here
