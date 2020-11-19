(uiop:define-package :pacman-cl/src/main
  (:use :cl :pacman-cl/src/entities)
  (:local-nicknames (#:dcl #:defclass-std) (#:gk #:trivial-gamekit))
  (:export :foo))

(in-package :pacman-cl/src/main)

(gk:defgame pacman-game () ()
  (:viewport-width 800)     ; window's width
  (:viewport-height 600); window's height
  (:viewport-title "Pacman"))

(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *origin* (gamekit:vec2 0 0))

(defmethod gamekit:draw ((app pacman-game))
  ;; Let's draw a black box in the bottom-left corner
  (gamekit:draw-rect *origin* 100 100 :fill-paint *black*))
