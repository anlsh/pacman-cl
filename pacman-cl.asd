;;;; pacman.asd

(asdf:defsystem #:pacman-cl
  :description "A common lisp clone of pacman"
  :author "Anish Moorthy (anlsh@protonmail.com)"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "pacman-cl")))
