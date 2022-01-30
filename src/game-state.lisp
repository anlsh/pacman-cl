(uiop:define-package :pacman-cl/src/game-state
  (:use :cl :pacman-cl/src/entities)
  (:local-nicknames (#:alx #:alexandria)
                    (#:gk #:trivial-gamekit)
                    (#:json #:jonathan)
                    (#:dcl #:defclass-std))
  (:export
   #:bind-input-handler/active-game
   #:init-game-state
   #:step-game
   #:game-map
   #:player-char
   #:tick))

(in-package :pacman-cl/src/game-state)

(defclass game-state ()
  ((is-paused :accessor is-paused :initform nil)
   (player-char :accessor player-char :initarg :player-char)
   (enemies :accessor enemies :initarg :enemy-list)
   (game-map :accessor game-map :initarg :game-map)
   (tick :accessor tick :initform 0)))

(defun bind-input-handler/active-game (game-state)
  ;; TODO This event-based handling is GONNA bite me in the butt...
  (declare (type game-state game-state))
  (with-slots (player-char) game-state
    (gk:bind-button :p :pressed (lambda ()
                                  (setf (is-paused game-state)
                                        (not (is-paused game-state)))))
    (gk:bind-button :q :pressed #'gk:stop)
    (gk:bind-button :w :pressed (lambda () (setf (next-dir player-char) :up)))
    (gk:bind-button :a :pressed (lambda () (setf (next-dir player-char) :left)))
    (gk:bind-button :s :pressed (lambda () (setf (next-dir player-char) :down)))
    (gk:bind-button :d :pressed (lambda () (setf (next-dir player-char) :right)))))

(defun init-game-state (map-filename)
  (let* ((map-file-contents (alx:read-file-into-string map-filename))
         (map-json (json:parse map-file-contents :as :hash-table))
         (map-array
           (let ((lines (split-sequence:split-sequence #\newline (gethash "map-layout" map-json))))
             (make-array (list (length lines) (length (elt lines 0)))
                         :initial-contents lines))))
    (make-instance 'game-state
                   :game-map map-array
                   :player-char (apply #'make-pacman (gethash "player-spawn-loc" map-json))
                   :enemy-list '())))

(defmethod step-game ((state game-state))
  (if (is-paused state)
      (return-from step-game nil))
  (let ((player (player-char state))
        (game-map (game-map state)))
    ;; Set the player's direction
    (with-accessors ((dx dx) (dy dy)) player
      (ecase (dir player)
        (:up (setf dx 0 dy .25))
        (:down (setf dx 0 dy -0.25))
        (:left (setf dx -0.25 dy 0))
        (:right (setf dx 0.25 dy 0))))
    ;; Check for player collisions with walls, kill velocity if so
    (with-accessors ((x x) (y y) (dx dx) (dy dy)) player
      (cond ((> dx 0) (if (equalp #\b (aref game-map (round y) (ceiling (+ x dx))))
                          (setf (dx player) 0)))
            ((< dx 0) (if (equalp #\b (aref game-map (round y) (floor (+ x dx))))
                          (setf (dx player) 0)))
            ((> dy 0) (if (equalp #\b (aref game-map (ceiling (+ y dy)) (round x)))
                          (setf (dy player) 0)))
            ((< dy 0) (if (equalp #\b (aref game-map (floor (+ y dy)) (round x)))
                          (setf (dy player) 0))))
      ;; Move the player if they can indeed move
      (incf x dx)
      (incf y dy)))
  (with-slots (tick) state
    (incf tick)))
