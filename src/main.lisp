(defpackage :pacman-cl/src/main
  (:use :cl)
  (:local-nicknames (#:gk #:trivial-gamekit))
  (:export :foo))

(in-package :pacman-cl/src/main)

(defun foo () 4)
