(uiop:define-package :pacman-cl/src/graphics
  (:use :cl :pacman-cl/src/game-state :pacman-cl/src/entities)
  (:local-nicknames (#:alx #:alexandria) (#:gk #:trivial-gamekit))
  (:export #:draw-game))

(in-package :pacman-cl/src/graphics)

;; Number of pixels a single box will occupy
(defvar *unit-size* 20)

;; Colors
(defvar *map-line-thickness* 5)
(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *blue* (gamekit:vec4 0 0 1 1))
(defvar *yellow* (gamekit:vec4 1 1 0 1))

(defun draw-grid-box (x y color)
  (gk:draw-rect (gk:vec2 (* x *unit-size*) (* y *unit-size*))
                *unit-size* *unit-size* :fill-paint color))

(defun draw-game (game-state)
  (draw-map (game-map game-state))
  (draw-player (player-char game-state)))

(defun draw-player (player-char)
  (gk:draw-rect (gk:vec2  (* (x player-char))
                          (* (y player-char)))
                *unit-size* *unit-size*
                :fill-paint *yellow*))

(defun draw-map (game-map)
  ;; Draw a black screen first
  (gk:draw-rect (gk:vec2 0 0) 3000 3000 :fill-paint *black*)
  ;; Draw the map
  (loop with map-width = (elt (array-dimensions game-map) 1)
        with map-height = (elt (array-dimensions game-map) 0)
        for x-index from 0 below map-width do
          (loop for y-index below map-height
                for map-item = (aref game-map y-index x-index)
                do
                   (gk:with-pushed-canvas ()
                     (gk:translate-canvas (* x-index *unit-size*)
                                          (* y-index *unit-size*))
                     (case map-item
                       (#\b (draw-barrier game-map x-index y-index))
                       (otherwise (draw-grid-box 0 0 *black*)))))))

(defun draw-barrier (game-map x-index y-index)
  (flet ((fits-pattern (pattern) (fits-pattern game-map x-index y-index pattern)))
     (macrolet
         ((draw-with-rotation-on-fit (&body clauses)
            ;; Each "clause" consists of a (pattern implicit-progn...). When a clause is matched
            ;; for the current position the corresponding progn is triggered and upon completion
            ;; the draw-barrier function is terminated
            (alx:with-gensyms (fits rotation-code)
              (append '(progn)
                      (mapcar (lambda (clause)
                                (destructuring-bind (pattern . body) clause
                                  `(multiple-value-bind (,fits ,rotation-code)
                                       (fits-pattern ,pattern)
                                     (if ,fits
                                         (gk:with-pushed-canvas ()
                                           (gk:rotate-canvas (* -1 ,rotation-code (/ pi 2)))
                                           (progn ,@body (return-from draw-barrier)))))))
                              clauses)))))
       (draw-with-rotation-on-fit
        (#(:e :e :e :a :a :a :a :a)
          (gk:draw-arc (gk:vec2 0 0)
                       (/ *unit-size* 2) 0 (/ pi 2)
                       :stroke-paint *blue* :thickness *map-line-thickness*))
        (#(:a :a :a :a :a :a :a :a)
          (gk:draw-rect (gk:vec2 (* -1 (/ *unit-size* 2)) (* -1 (/ *unit-size* 2)))
                        *unit-size* *unit-size* :fill-paint *blue*))))))

;; Tools for working with border patterns
;; Standard order is immediate right then counterclockwise from there
(defvar *standard-ordered-offs*
  (vector '(1 0) '(1 1) '(0 1) '(-1 1) '(-1 0) '(-1 -1) '(0 -1) '(1 -1)))

(defun fits-pattern (game-map x-index y-index neighborhood-pattern)
  "Given an index on the game map, check if the neighborhood of <x-index, y-index> looks
like the given pattern up to rotation. Returns two values
1. t if the neighborhood-pattern matches the neighborhood up to rotation, nil if not
2. 1 if the (first) matching rotation of the neighborhood pattern is 90 degrees, 2 for 180, etc
   Undefined when the neighborhood and pattern are incogruous

The pattern must be an 8-element vector composed of the :b, :e, and :a symbols. :b matches a
barrier, :e matches non-barriers (and invalid coordinates), and :a matches everything matched by :b
and :e"
  (labels ((matches (pattern-index xoff yoff)
             (if (not (and (< -1 (+ x-index xoff) (elt (array-dimensions game-map) 1))
                           (< -1 (+ y-index yoff) (elt (array-dimensions game-map) 0))))
                 (equalp :a (aref neighborhood-pattern pattern-index))
                 (ecase (aref neighborhood-pattern pattern-index)
                   (:a t)
                   (:e (not (equalp #\b (aref game-map (+ y-index yoff) (+ x-index xoff)))))
                   (:b (equalp #\b (aref game-map (+ y-index yoff) (+ x-index xoff))))))))

    (loop for vector-rot below 4 do
            (labels ((get-rotated-off (off-idx)
                       (aref *standard-ordered-offs* (mod (- off-idx (* 2 vector-rot))
                                                          (length *standard-ordered-offs*)))))
              (loop for i below (length *standard-ordered-offs*)
                    for rotated-off = (get-rotated-off i)
                    always (apply #'matches i rotated-off)
                    finally (return-from fits-pattern (values t vector-rot))))
          finally (return-from fits-pattern (values nil nil)))))
