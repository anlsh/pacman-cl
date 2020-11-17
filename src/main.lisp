(uiop:define-package :pacman-cl/src/main
  (:use :cl)
  (:export :foo))

(in-package :pacman-cl/src/main)

(defun foo () 4)
