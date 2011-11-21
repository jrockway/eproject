;;; Author: Antono Vasiljev <self@antono.info>

;; Ruby on Rails project
(define-project-type ruby-on-rails (generic)
  (and (look-for "Gemfile") (look-for "config/application.rb"))
  :relevant-files ("\.rb$"
                   "\.coffee$"
                   "\.js$"
                   "\.sass$"
                   "\.scss$"
                   "\.css$"
                   "\.less$"
                   "\.haml$"
                   "\.slim$"
                   "\.erb$"
                   "\.ru$"
                   "\.yml$"
                   "\.yaml$"
                   "\.feature$"
                   "Gemfile$"
                   "Capfile$"
                   "\.ru$"
                   "README\..*$")
  :irrelevant-files ("tmp/.*"
                     "log/.*")
  :main-file "Gemfile")

(provide 'eproject-ruby-on-rails)
