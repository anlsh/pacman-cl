(uiop:define-package :pacman-cl/src/game-state
  (:use :cl :pacman-cl/src/entities)
  (:local-nicknames (#:alx #:alexandria)
                    (#:gk #:trivial-gamekit)
                    (#:json #:jonathan)
                    (#:dcl #:defclass-std))
  (:export
   #:bind-input-handler/active-game
   #:init-game-state
   #:game-step
   #:player-char))

(in-package :pacman-cl/src/game-state)

(dcl:defclass/std game-state ()
  ((player-char
    enemy-list
    game-map)))

(defclass game-state ()
  ((player-char :accessor player-char :initarg :player-char)
   (enemies :accessor enemies :initarg :enemy-list)
   (game-map :accessor game-map :initarg :game-map)))

(defun bind-input-handler/active-game (game-state)
  ;; TODO This event-based handling is GONNA bite me in the butt...
  (declare (type game-state game-state))
  (with-slots (player-char) game-state
    (gk:bind-button :w :pressed (lambda () (setf (dir player-char) :up)))
    (gk:bind-button :a :pressed (lambda () (setf (dir player-char) :left)))
    (gk:bind-button :s :pressed (lambda () (setf (dir player-char) :down)))
    (gk:bind-button :d :pressed (lambda () (setf (dir player-char) :right)))))

(defun init-game-state (map-filename)
  (let* ((map-file-contents (alx:read-file-into-string map-filename))
         (map-json (json:parse map-file-contents :as :hash-table))
         (map-array
           (let ((lines (split-sequence:split-sequence #\newline (gethash "map-layout" map-json))))
             (make-array (list (length lines) (length (elt lines 0)))
                         :initial-contents lines))))
    (make-instance 'game-state
                   :game-map map-array
                   :player-char (apply #'make-pacman
                                       (append (gethash "player-spawn-loc" map-json)
                                               '(:right)))
                   :enemy-list '())))

(defmethod game-step ((state game-state))
  (let ((player (player-char state)))
    (ecase (dir player)
      (:left (incf (x player) -1))
      (:right (incf (x player) 1))
      (:up (incf (y player) 1))
      (:down (incf (y player) -1)))))
