;;;; util.lisp
(cl:in-package :deserted)

(defparameter *viewport-width* 800)
(defparameter *viewport-height* 600)
(defparameter *viewport-origin* (vec2 0 0))

(defparameter *white* (vec4 1 1 1 1))
(defparameter *black* (vec4 0 0 0 1))

(defparameter *assets-path* (asdf:system-relative-pathname :deserted "assets/"))

;;; Rendering

(defgeneric render (object)
  (:method (object) (declare (ignore object))))

(defclass renderable () ())

(defmethod render :around ((this renderable))
  (with-pushed-canvas ()
    (call-next-method)))

;;; Positioning

(defgeneric (setf position-of) (vec2 positionable))

(defclass positionable ()
  ((position
    :initform (vec2 0 0) :initarg :position :reader position-of)))

(defmethod (setf position-of) ((vec vec2) (this positionable))
  (setf (x (position-of this)) (x vec)
        (y (position-of this)) (y vec)))
