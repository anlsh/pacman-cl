(uiop:define-package :pacman-cl/src/main
  (:use :cl :pacman-cl/src/game-state :pacman-cl/src/entities :pacman-cl/src/graphics)
  (:local-nicknames (#:dcl #:defclass-std) (#:gk #:trivial-gamekit))
  (:export :foo))

(in-package :pacman-cl/src/main)

(gk:defgame pacman-game ()
  ((game-state :reader game-state
               :initform (init-game-state "../resources/classic.map.json"))
   (graphics-state :reader graphics-state))
  (:viewport-width 800)     ; window's width
  (:viewport-height 600); window's height
  (:viewport-title "Pacman")
  (:act-rate 16))

(defmethod gk:post-initialize ((app pacman-game))
  (bind-input-handler/active-game (game-state app))
  (gk:register-resource-package :pacman-cl/images "/home/anish/Code/pacman-cl/resources/"))

(defmethod gk:act ((app pacman-game))
  (step-game (game-state app)))

(defmethod gk:draw ((app pacman-game))
  (draw-game (game-state app)))
