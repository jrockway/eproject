;; eproject.el - assign files to projects, programatically
;;
;; Copyright (C) 2008, 2009 Jonathan Rockway <jon@jrock.us>
;;
;; Author: Jonathan Rockway <jon@jrock.us>
;; Maintainer: Jonathan Rockway <jon@jrock.us>
;; Created: 20 Nov 2008
;; Version: 1.1
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
;;
;;; Commentary
;;
;; Eproject is an extension that lets you group related files together
;; as projects.  It aims to be as unobtrusive as possible -- no new
;; files are created (or required to exist) on disk, and buffers that
;; aren't a member of a project are not affected in any way.
;;
;; The main starting point for eproject is defining project types.
;; There is a macro for this, define-project-type, that accepts four
;; arguments, the type name (a symbol), a list of supertypes (for
;; inheriting properties) a form that is executed to determine whether
;; a file is a member of a project, and then a free-form property
;; list.  An example will clear things up.
;;
;; Let's create a "perl" project type, for Perl projects that have a
;; Makefile.PL.
;;
;; (define-project-type perl (generic)
;;   (look-for "Makefile.PL")
;;   :relevant-files ("\\.pm$" "\\.t$"))
;;
;; Now when you open a file and somewhere above in the directory tree
;; there is a Makefile.PL, it will be a "perl project".
;;
;; There are a few things you get with this.  A hook called
;; perl-project-file-visit-hook will be run, and the buffer will have
;; the "eproject-mode" minor-mode turned on.  You can also read and
;; set metadata via the eproject-get-project-metadatum and
;; eproject-add-project-metadatum calls.
;;
;; (Right now, you get a "C-c C-f" keybinding which will allow you
;; to easily visit any of the "relevant files" in the project.  You
;; can add your own bindings to the eproject-mode-map, of course.)
;;
;; Let's look at the mechanics of the define-project-type call.  The
;; first argument is the name of the project type -- it can be any
;; symbol.  The next argument is a list of other projects types that
;; this project will inherit from.  That means that if you call
;; eproject-get-project-metadatum and the current project doesn't
;; define a value, we'll look at the supertypes until we get something
;; non-nil.  Usually you will want to set this to (generic), which
;; will make your type work correctly even if you don't define any of
;; your own metadata.
;;
;; The next argument is a form that will be executed with the filename
;; that was just opened bound to FILE.  It is expected to return the
;; project root, or nil if FILE is not in a project of this type.  The
;; look-for function will look up the directory tree for a file that
;; is named the same as its argument (see the docstring for
;; `eproject--look-for-impl' for all the details).  You can write any
;; Lisp here you like; we'll see some more examples later.  (You only
;; get one form, so if you need to execute more than one, just wrap it
;; in a progn.)
;;
;; The final (&rest-style) argument is a property list of initial project
;; metadata.  You can put anything you want here, as long as it is in the
;; form of a property list (keyword, value, keyword, value, ...).
;;
;; After this form runs, eproject will be able to recognize files in
;; the type of the project you defined.  It also creates a hook named
;; <type>-project-file-visit-hook.  You can do anything you want here,
;; including access (eproject-type) and (eproject-root).
;;
;; As an example, in my perl-project-file-visit-hook, I do this:
;;
;; (lambda ()
;;   (ignore-errors
;;     (stylish-repl-eval-perl
;;      (format "use lib '%s'" (car (perl-project-includes)))))))
;;
;; This will add the library directory of this project to my current
;; stylish-repl session, so that I can use my project in the REPL
;; immediately.  (I do something similar for Lisp + SLIME projects)
;;
;; That's basically all there is.  eproject is designed to be minimal and
;; extensible, so I hope it meets your needs.
;;
;; Please do not hesitate to e-mail or find me on #emacs (jrockway) if
;; you have questions.  If you'd like to send a patch (always appreciated),
;; please diff against the latest git version, available by running:
;;
;; $ git clone git://github.com/jrockway/eproject
;;
;; Share and enjoy.
;;
;; Bugs:
;;
;; You can't forward reference supertypes -- this will mess things up
;; internally, but you won't get a warning.  This can be easily fixed
;; by using a smarter algorithm for eproject--all-types.
;;
;; The "linearized isa" (i.e. "class precedence list") is computed
;; with a depth-first search.  This is bad; we should really use the
;; C3 ordering.
;;
;; Website:
;;
;; The latest version is on github at
;; http://github.com/jrockway/eproject/tree/master
;;
;;; Changelog:
;;
;; 1.1 (Sat Jan 31 20:03:56 CST 2009)
;;
;; * Make the completing-read function customizable
;;
;; 1.0 (Nov 28 2008)
;;
;; * Initial release
;;

;;; Code:

(require 'iswitchb)
(require 'cl)

(defgroup eproject nil "eproject" :prefix "eproject-")

(defvar eproject-project-types nil
  "An alist of PROJECT to (supertypes selector metadata-plist) pairs.")

(defmacro define-project-type (type supertypes selector &rest metadata)
  "Define a new project type TYPE that inherits from SUPERTYPES.
SELECTOR is a form that is given a filename FILE and returns the
project root if it is of this type of project, or NIL otherwise."
  `(progn
     (defvar ,(intern (format "%s-project-file-visit-hook" type)) nil
       ,(format "Hooks that will be run when a file in a %s project is opened." type))
       (setq eproject-project-types
             (nconc (assq-delete-all ',type eproject-project-types)
                    (list
                     (list ',type ',supertypes
                           (lambda (file) ,selector)
                           ',metadata))))))

(defun eproject--scan-parents-for (start-at predicate)
  "Call PREDICATE with each parent directory of START-AT, returning the
path to the first directory where PREDICATE returns T."
  (cond ((funcall predicate start-at) start-at)
        ((not (equal start-at "/"))
         (eproject--scan-parents-for
          (expand-file-name (concat start-at "/" "..")) predicate))
        (t nil)))

(defun eproject--find-file-named (start-at filename)
  (eproject--scan-parents-for start-at
   (lambda (directory)
     (file-exists-p (concat directory "/" filename)))))

(define-project-type generic () nil :relevant-files ("^[^.]"))
(define-project-type generic-git (generic) (look-for ".git"))

(defun eproject--type-info (type)
  (or
   (assoc type eproject-project-types)
   (error "No type %s" type)))

(defun eproject--project-supertypes (type)
  (nth 1 (eproject--type-info type)))

(defun eproject--project-selector (type)
  (nth 2 (eproject--type-info type)))

(defun eproject--look-for-impl (file expression &optional type)
  "Implements the LOOK-FOR function that is flet-bound during
`eproject--run-project-selector'.  EXPRESSION and TYPE specify
what to look for.  Some examples:

   (look-for \"Makefile.PL\") ; look up the directory tree for a file called Makefile.PL
   (look-for \"*.PL\" :glob) ; look for a file matching *.PL
"
  (when (not type) (setq type :filename))
  (case type
    (:filename (eproject--find-file-named file expression))
    (:glob (eproject--scan-parents-for file
             (lambda (current-directory)
               (let ((default-directory current-directory))
                 (and (not (equal file current-directory))
                      (> (length (file-expand-wildcards expression)) 0))))))
    (otherwise (error "Don't know how to handle %s in LOOK-FOR!" type))))

(defun* eproject--run-project-selector (type &optional (file (buffer-file-name)))
  "Run the selector associated with project type TYPE."
  (flet ((look-for (expr &optional expr-type)
                   (funcall #'eproject--look-for-impl file expr expr-type)))
    (funcall (eproject--project-selector type) file)))

(defun eproject--linearized-isa (type &optional include-self)
  (delete-duplicates
   (nconc
    (if include-self (list type))
    (eproject--project-supertypes type)
    (loop for stype in (eproject--project-supertypes type)
          nconc (eproject--linearized-isa stype)))))

(defun eproject--all-types ()
  ;; this should be most specific to least specific, as long as nothing
  ;; is forward-referenced.
 (reverse (mapcar #'car eproject-project-types)))

(defun eproject-get-project-metadatum (type key)
  (loop for next-type in (eproject--linearized-isa type t)
        do (let ((result (getf (nth 3 (eproject--type-info next-type)) key)))
             (if result (return result)))))

(defun eproject-add-project-metadatum (type key value)
  (setf (getf (nth 3 (assoc type eproject-project-types)) key) value))

(defvar eproject-root nil
  "A buffer-local variable set to the root of its eproject project.  NIL if it isn't in an eproject.")

(defvar eproject-type nil
  "A buffer-local variable set to the type of this buffer's eproject project.  NIL if the buffer isn't in an eproject.")

(make-variable-buffer-local 'eproject-root)
(make-variable-buffer-local 'eproject-type)

(defmacro define-eproject-accessor (variable)
  "Create a function named eproject-VARIABLE that returns the value of VARIABLE in the context of the current project."
  (let ((sym (intern (format "eproject-%s" variable))))
  `(defun* ,sym
       (&optional (buffer (current-buffer)))
     ,(format "Return the value of the eproject variable %s.  BUFFER defaults to the current buffer." variable)
     (with-current-buffer buffer
       (when (not eproject-mode)
         (error "Buffer is not an eproject buffer!"))
       ,sym))))

(define-eproject-accessor root)
(define-eproject-accessor type)

(define-minor-mode eproject-mode
  "A minor mode for buffers that are a member of an eproject project."
  nil " Project"
  '(("" . eproject-ifind-file))
  (when (null eproject-root)
    (error "Please do not use this directly.  Call eproject-maybe-turn-on instead.")))

(defun eproject-maybe-turn-on ()
  "Turn on eproject for the current buffer, if it is in a project."
  (interactive)
  (loop for type in (eproject--all-types)
        do (let ((root (eproject--run-project-selector type)))
             (when root
               (setq eproject-type type)
               (setq eproject-root root)
               (eproject-mode 1)
               (run-hooks (intern (format "%s-project-file-visit-hook" type)))
               (return root)))))

(defun eproject--search-directory-tree (directory file-regexp ignore-regexp)
  (loop for file in (directory-files (file-name-as-directory directory) t "^[^.]" t)
        when (and (not (file-directory-p file))
                  (not (string-match ignore-regexp file))
                  (string-match file-regexp file))
        collect file into files
        when (file-directory-p file)
        collect file into directories
        finally return
          (nconc files
                 (loop for dir in directories
                       nconc (eproject--search-directory-tree dir file-regexp
                                                              ignore-regexp)))))

(defun eproject--shorten-filename (filename)
  (string-match (format "^%s/\\(.+\\)$" (regexp-quote (eproject-root))) filename)
  (cons (match-string 1 filename) filename))

(defun eproject-assert-type (type)
  "Assert that the current buffer is in a project of type TYPE."
  (when (not (memq type (eproject--linearized-isa (eproject-type) t)))
    (error (format "%s is not in a project of type %s!"
                   (buffer-file-name) type))))

;; support for visiting other project files

(defun eproject--completing-read (prompt choices)
  "Use completing-read to do a completing read."
  (completing-read prompt choices nil t))

(defun eproject--icompleting-read (prompt choices)
  "Use iswitchb to do a completing read."
  (let ((iswitchb-make-buflist-hook
         (lambda ()
           (setq iswitchb-temp-buflist choices))))
    (when (not iswitchb-mode)
      (add-hook 'minibuffer-setup-hook 'iswitchb-minibuffer-setup))
    (prog1 (iswitchb-read-buffer prompt nil t)
      (when (not iswitchb-mode)
        (remove-hook 'minibuffer-setup-hook 'iswitchb-minibuffer-setup)))))

(defun eproject--ido-completing-read (prompt choices)
  "Use ido to do a completing read."
  (ido-completing-read prompt choices nil t))

(defcustom eproject-completing-read-function
  #'eproject--icompleting-read
  "The function used to ask the user select a single file from a
list of files; used by `eproject-find-file'."
  :group 'eproject
  :type '(radio (function-item :doc "Use emacs' standard completing-read function."
                               eproject--completing-read)
                (function-item :doc "Use iswitchb's completing-read function."
                               eproject--icompleting-read)
                (function-item :doc "Use ido's completing-read function."
                               eproject--ido-completing-read)
                (function)))

(defun eproject--icomplete-read-with-alist (prompt alist)
  (let ((show (mapcar (lambda (x) (car x)) alist)))
    (cdr (assoc (funcall eproject-completing-read-function prompt show) alist))))

(defalias 'eproject-ifind-file 'eproject-find-file)  ;; ifind is deperecated
(defun eproject-find-file ()
  (interactive)
  (let ((matcher (format "\\(?:%s\\)"
                         (reduce (lambda (a b) (concat a "\\|" b))
                                 (mapcar (lambda (f) (format "\\(?:%s\\)" f))
                                         (eproject-get-project-metadatum
                                          (eproject-type) :relevant-files)))))
        (ignore (concat (regexp-opt completion-ignored-extensions t) "$")))
    (find-file (eproject--icomplete-read-with-alist
                "Project file: "
                (mapcar #'eproject--shorten-filename
                        (eproject--search-directory-tree (eproject-root) matcher ignore))))))

;; finish up

(add-hook 'find-file-hook #'eproject-maybe-turn-on)

(provide 'eproject)

;;; eproject.el ends here
