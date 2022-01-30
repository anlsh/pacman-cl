(uiop:define-package :pacman-cl/src/entities
  (:use :cl)
  (:local-nicknames (#:dcl #:defclass-std))
  (:export
   #:x
   #:y
   #:dx
   #:dy
   #:dir
   #:make-pacman
   #:speed
   #:get-angle))

(in-package :pacman-cl/src/entities)

;; Position stuff

(defclass positionable ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))

(defclass moveable ()
  ((dx :accessor dx :initarg :dx)
   (dy :accessor dy :initarg :dy)))

;; Orientation stuff

(defvar *directions* '(:up :down :left :right))

(defclass orientable ()
  ((dir :accessor dir :initarg :dir)))

(defmethod get-angle ((o orientable))
  (ecase (dir o)
    (:up 90)
    (:down 270)
    (:right 0)
    (:left 180)))

(defmethod (setf dir) (new-dir (o orientable))
  (unless (find new-dir *directions*)
    (error "~a is not one of ~a" new-dir *directions*))
  (setf (slot-value o 'dir) new-dir))

(defmethod initialize-instance :after ((o orientable) &key dir)
  (setf (dir o) dir))

(defmethod (setf dir) (new-dir (o entity/pacman))
  (unless (find new-dir *directions*)
    (error "~a is not one of ~a" new-dir *directions*))
  (setf (slot-value o 'old-dir) (dir o)
        (slot-value o 'dir) new-dir))

(defclass entity/pacman (moveable orientable)
  ((next-dir :accessor next-dir :initarg :next-dir)))

(defun make-pacman (x y)
  (make-instance 'entity/pacman
                 :x x :y y :dx 0.25 :dy 0 :next-dir :right :dir :right))
