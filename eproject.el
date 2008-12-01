;;; eproject

(require 'iswitchb)
(require 'cl)

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
  "Look for a file named FILE in parent directories of START-AT"
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

(defun* eproject--run-project-selector (type &optional (file (buffer-file-name)))
  (flet ((look-for (filename)
                   (eproject--find-file-named file filename)))
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
  "A buffer-local variable set to the root of its eproject project.  NIL if
it isn't in an eproject.")

(defvar eproject-type nil
  "A buffer-local variable set to the type of this buffer's eproject project.  NIL
if the buffer isn't in an eproject.")

(make-variable-buffer-local 'eproject-root)
(make-variable-buffer-local 'eproject-type)

(defmacro define-eproject-accessor (variable)
  `(defun* ,(intern (format "eproject-%s" variable))
       (&optional (buffer (current-buffer)))
     ,(format "Return the value of the eproject variable %s.  BUFFER defaults to the current buffer." variable)
     (with-current-buffer buffer
       (when (not eproject-mode)
         (error "Buffer is not an eproject buffer!"))
       ,(intern (format "eproject-%s" variable)))))

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

(defun eproject--search-directory-tree (directory file-regexp)
  (let* ((content (directory-files (file-name-as-directory directory) t "^[^.]" t))
         (files (loop for file in content
                      when (and (not (file-directory-p file))
                                (string-match file-regexp file))
                      collect file))
         (directories (loop for file in content
                            when (file-directory-p file)
                            collect file)))
    (nconc files
           (loop for dir in directories
                 nconc (eproject--search-directory-tree dir file-regexp)))))

(defun eproject--shorten-filename (filename)
  (string-match (format "^%s/\\(.+\\)$" (regexp-quote (eproject-root))) filename)
  (cons (match-string 1 filename) filename))

(defun eproject--icompleting-read (prompt choices)
  "Use iswitch as a completing-read replacement to choose from
choices.  PROMPT is a string to prompt with.  CHOICES is a list of
strings to choose from."
  (let ((iswitchb-make-buflist-hook
         (lambda ()
           (setq iswitchb-temp-buflist choices))))
    (iswitchb-read-buffer prompt)))

(defun eproject--icomplete-read-with-alist (prompt alist)
  (let ((show (mapcar (lambda (x) (car x)) alist)))
    (cdr (assoc (eproject--icompleting-read prompt show) alist))))

(defun eproject-ifind-file ()
  (interactive)
  (let ((matcher (format "\\(?:%s\\)"
                         (reduce (lambda (a b) (concat a "\\|" b))
                                 (mapcar (lambda (f) (format "\\(?:%s\\)" f))
                                         (eproject-get-project-metadatum
                                          (eproject-type) :relevant-files))))))
    (find-file (eproject--icomplete-read-with-alist
                "Project file: "
                (mapcar #'eproject--shorten-filename
                        (eproject--search-directory-tree (eproject-root) matcher))))))

(defun eproject-assert-type (type)
  "Assert that the current buffer is in a project of type TYPE."
  (when (not (memq type (eproject--linearized-isa (eproject-type) t)))
    (error (format "%s is not in a project of type %s!"
                   (buffer-file-name) type))))

(add-hook 'find-file-hook #'eproject-maybe-turn-on)

(provide 'eproject)
