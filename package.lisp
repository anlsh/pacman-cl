(uiop:define-package :pacman-cl/package
  (:nicknames :pacman-cl)
  (:use #:cl)
  (:use-reexport :pacman-cl/src/package)
  (:export
   #:boo))

(in-package :pacman-cl/package)
