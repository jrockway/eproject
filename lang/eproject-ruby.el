;;; Author: Antono Vasiljev <self@antono.info>

(define-project-type ruby (generic)
  (or (look-for "Rakefile")
      (look-for "Gemfile")
      (look-for ".rmvrc")))

(provide 'eproject-ruby)
