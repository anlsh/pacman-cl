;;;; pacman.asd

(asdf:defsystem :pacman-cl
  :description "A common lisp clone of pacman"
  :author "Anish Moorthy (anlsh@protonmail.com)"
  :license  "MIT"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:defclass-std :uiop :jonathan :trivial-gamekit
                :fset
                :alexandria
                :pacman-cl/package))
