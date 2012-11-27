;;; Author: Antono Vasiljev <self@antono.info>

;; Arduino project
(define-project-type arduino (generic)
  (look-for "ino.ini")   ;; http://inotool.org/
  :main-file "ino.ini")

(provide 'eproject-arduino)
