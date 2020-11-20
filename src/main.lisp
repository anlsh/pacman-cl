(uiop:define-package :pacman-cl/src/main
  (:use :cl :pacman-cl/src/game-state :pacman-cl/src/entities)
  (:local-nicknames (#:dcl #:defclass-std) (#:gk #:trivial-gamekit))
  (:export :foo))

(in-package :pacman-cl/src/main)

(gk:defgame pacman-game ()
  ((game-state :reader game-state
               :initform (init-game-state "../resources/classic.map.json")))
  (:viewport-width 800)     ; window's width
  (:viewport-height 600); window's height
  (:viewport-title "Pacman"))

(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *origin* (gamekit:vec2 0 0))

(defmethod gk:post-initialize ((app pacman-game))
  (bind-input-handler/active-game (game-state app)))

(defmethod gk:act ((app pacman-game))
  (game-step (game-state app)))

(defmethod gk:draw ((app pacman-game))
  (let ((player-char (player-char (game-state app))))
    (gamekit:draw-rect (gk:vec2 (x player-char) (y player-char))
                       100 100 :fill-paint *black*)))
