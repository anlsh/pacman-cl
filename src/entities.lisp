(uiop:define-package :pacman-cl/src/entities
  (:use :cl)
  (:local-nicknames (#:dcl #:defclass-std))
  (:export
   #:positionable/get-x
   #:positionable/get-y
   #:orientable/get-orientation
   #:orientable/set-orientation))

(in-package :pacman-cl/src/entities)

;; Position stuff

(dcl:defclass/std positionable ()
  ((x y)))

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


(defclass orientable ()
  ((dir :accessor dir :initarg :dir)))

(defmethod (setf dir) (new-dir (o orientable))
  (unless (find new-dir *directions*)
    (error "~a is not one of ~a" new-dir *directions*))
  (setf (slot-value o 'dir) new-dir))

(defmethod intialize-instance :after ((o orientable) &key dir)
  (setf (dir o) dir))

;; Some actual specializations

(dcl:defclass/std entity/pacman (positionable orientable)
  ())

(defun make-pacman (x y dir)
  (make-instance 'entity/pacman :x x :y y :dir dir))
