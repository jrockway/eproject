;;; eproject-find-cmd.el --- use find (via find-cmd) to define project files
;;
;; Copyright (C) 2013 Jeff Leverenz <jeff.leverenz@gmail.com>
;;
;; Author: Jeff Leverenz <jeff.leverenz@gmail.com>
;; Keywords: programming, projects
;;
;; This file is not a part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Combine find-cmd with eproject to define your eproject files.
;;
;; For large projects, eproject-extras will sometimes try to construct very
;; long command line strings that bash cannot handle. e.g.:
;;
;;     /bin/bash: /usr/bin/grep: Argument list too long
;;     Grep exited abnormally with code 126 ...
;;
;; This seems mostly due to use of the eproject-list-project-files* related
;; functions, which can build very long strings.
;;
;; As an alternative, using find provides a way to operate/use a project's file
;; list for many "extras" while still maintaining many other core eproject
;; usefulness.
;;
;; To use eproject-find-cmd, your project definition must contain the attribute
;; :find-cmd-sexp, which is an sexp for find-cmd specifying the project's
;; files.  For example, if you'd like a project defined to be all *.c and *.h files:
;;
;;     (define-project-type find-cmd-ch-project (generic)
;;       (look-for ".find-cmd-ch-project")
;;       :find-cmd-sexp (and (type "f") (or (name "*.h") (name "*.c"))))

;;; Code:

(require 'find-cmd)

(defun build-cd-and-find-cmd (dir find-cmd-sexp)
  "Builds a command line with format \"cd <dir> && find. <args>\".
This makes the find output paths relative to DIR (thus shorter)
for readability in tools like anything & find-grep"
  (let ((root-directory dir)
        (default-directory "."))
    (concat "cd " root-directory " && " (find-cmd find-cmd-sexp))))

(defun eproject-find-cmd--sexp-or-default ()
  (let ((find-cmd-sexp (eproject-attribute :find-cmd-sexp)))
    (if find-cmd-sexp find-cmd-sexp '(and (prune (name ".git"))
                                          (and (type "f"))))))

(defun eproject-find-cmd-find-grep (regexp)
  (interactive "sRegexp grep: ")
  (let ((find-cmd-sexp (eproject-find-cmd--sexp-or-default)))
    (find-grep (format "%s | \"xargs\" -0 grep -nH -e %s"
                       (concat (build-cd-and-find-cmd (eproject-root) find-cmd-sexp)
                               " -print0")
                       regexp))))

(defun build-anything-source-cmd (root find-sexp pattern)
  "Build a find command string using 'find-cmd and root
directory ROOT for use with an anything eproject extension.  The
command generated uses a base FIND-SEXP and further limits the
query to files the match PATTERN."
  (build-cd-and-find-cmd root
                         (list 'and
                               find-sexp
                               (list 'iregex (format ".*%s.*" pattern)))))

(defvar anything-source-eproject-files
  '((name . "Files in eproject")
    (init . (lambda ()
              (setq
               anything-source-eproject--root (eproject-root)
               anything-source-eproject--sexp (eproject-find-cmd--sexp-or-default))))
    (candidates . (lambda ()
                    (start-process-shell-command
                     "file-search-process"
                     nil
                     (build-anything-source-cmd anything-source-eproject--root
                                                anything-source-eproject--sexp
                                                anything-pattern))))
    (type . file)
    (candidate-transformer . (lambda (l)
                               (mapcar (lambda (s)
                                         (cons s (concat anything-source-eproject--root s))) l)))
    (requires-pattern . 4)
    (volatile)
    (delayed)
    ))     ; C-M-x here to reset the defvar via eval-defun!

(provide 'eproject-find-cmd)
;;; eproject-find-cmd.el ends here
