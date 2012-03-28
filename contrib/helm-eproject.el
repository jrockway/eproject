;;; helm-eproject.el --- 
;; 
;; Filename: helm-eproject.el
;; Description: 
;; Author: Glauber Alex Dias Prado
;; Maintainer: 
;; Created: Ter Mar 27 20:26:06 2012 (-0300)
;; Version: 
;; Last-Updated: Ter Mar 27 21:11:45 2012 (-0300)
;;           By: Glauber Alex Dias Prado
;;     Update #: 7
;; URL: github
;; Keywords: emacs eproject helm complection
;; Compatibility: GNU Emacs 24.0.94.1
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; 
;; This small utility helps eproject to show the project files under helm
;; perhaps it would be better implemented using helm completion hooks but this
;; is working pretty good for now :).
;;
;;; Install:
;; 
;; Add the file somewhere to your load path like ~/elisp and add:
;; (require 'helm-eproject)
;; after you've setup both eproject and helm then just open a file that normal
;; eproject would pick up and voila :), you have to setup helm and eproject
;; for yourself so you can get the most out of these extensions.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;; helm support
(require 'helm)
(require 'eproject)
(require 'cl)

(defun helm-eproject-get-files ()
  (let ((matcher (format "\\(?:%s\\)"
                         (reduce (lambda (a b) (concat a "\\|" b))
                                 (mapcar (lambda (f) (format "\\(?:%s\\)" f))
                                         (eproject-get-project-metadatum
                                          (eproject-type) :relevant-files))))))
    (eproject--search-directory-tree (eproject-root) matcher)))

(defvar helm-eproject-source
  '((name . " helm-eproject: ")
    (init . (lambda ()
              (setq helm-eproject-last-buffer (current-buffer))))
    (type . file)
    (candidates . (lambda ()
                    (with-current-buffer helm-eproject-last-buffer (helm-eproject-get-files))))))

(defun helm-eproject ()
  "helps helm to use eproject to find a file"
  (interactive)
  (helm :sources '(helm-eproject-source)
        :buffer "*Helm Eproject*"))

(global-set-key [(control x) (f)] 'helm-eproject) ;;set-fill-column is just useless ;)

(provide 'helm-eproject)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-eproject.el ends here
