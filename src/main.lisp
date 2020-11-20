(uiop:define-package :pacman-cl/src/main
  (:use :cl :pacman-cl/src/game-state :pacman-cl/src/entities)
  (:local-nicknames (#:dcl #:defclass-std) (#:gk #:trivial-gamekit))
  (:export :foo))

(in-package :pacman-cl/src/main)

(gk:defgame pacman-game ()
  ((game-state :reader game-state
               :initform (init-game-state "../resources/classic.map.json"))
   (graphics-state :reader graphics-state))
  (:viewport-width 800)     ; window's width
  (:viewport-height 600); window's height
  (:viewport-title "Pacman"))

(defvar *origin* (gamekit:vec2 0 0))
(defvar *unit-size* 20)
(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *blue* (gamekit:vec4 0 0 1 1))
(defvar *yellow* (gamekit:vec4 1 1 0 1))

(defun draw-grid-box (x y color)
  (gk:draw-rect (gk:vec2 (* x *unit-size*) (* y *unit-size*))
                *unit-size* *unit-size* :fill-paint color))

(defmethod gk:post-initialize ((app pacman-game))
  (bind-input-handler/active-game (game-state app)))

(defmethod gk:act ((app pacman-game))
  (game-step (game-state app)))

(defmethod gk:draw ((app pacman-game))
  (let ((player-char (player-char (game-state app)))
        (game-map (game-map (game-state app))))
    (loop for x-index from 0 below (elt (array-dimensions game-map) 1) do
      (loop for y-index from 0 below (elt (array-dimensions game-map) 0) do
        (draw-grid-box x-index y-index
                       (case (aref game-map y-index x-index)
                         (#\b *blue*)
                         (otherwise *black*)))))
    (gamekit:draw-rect (gk:vec2 (x player-char) (y player-char))
                       *unit-size* *unit-size* :fill-paint *yellow*)))
