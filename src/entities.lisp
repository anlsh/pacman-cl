(uiop:define-package :pacman-cl/src/entities
  (:use :cl)
  (:local-nicknames (#:dcl #:defclass-std))
  (:export
   #:x
   #:y
   #:dir
   #:make-pacman
   #:speed))

(in-package :pacman-cl/src/entities)

;; Position stuff

(defclass positionable ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))

(defmethod (setf x) (new-val (p positionable))
  (declare (type fixnum new-val))
  (setf (slot-value p 'x) new-val))

(defmethod (setf y) (new-val (p positionable))
  (declare (type fixnum new-val))
  (setf (slot-value p 'y) new-val))

(defmethod initialize-instance :after ((p positionable) &key x y)
  (setf (x p) x)
  (setf (y p) y))

;; Orientation stuff

(defvar *directions* '(:up :down :left :right))

(defclass orientable ()
  ((dir :accessor dir :initarg :dir)))

(defmethod (setf dir) (new-dir (o orientable))
  (unless (find new-dir *directions*)
    (error "~a is not one of ~a" new-dir *directions*))
  (setf (slot-value o 'dir) new-dir))

(defmethod initialize-instance :after ((o orientable) &key dir)
  (setf (dir o) dir))

;; Some actual specializations

(defclass entity/pacman (positionable orientable)
  ((xspeed :accessor xspeed :initform 1)
   (want-dir :accessor want-dir :initarg :want-dir)))

(defun make-pacman (x y dir)
  (make-instance 'entity/pacman
                 :x x :y y :dir dir))
