;;;; util.lisp
(cl:in-package :deserted)

(defparameter *viewport-width* 800)
(defparameter *viewport-height* 600)
(defparameter *viewport-origin* (vec2 0 0))

(defparameter *white* (vec4 1 1 1 1))
(defparameter *black* (vec4 0 0 0 1))

(defparameter *assets-path* (asdf:system-relative-pathname :deserted "assets/"))

(alexandria:define-constant +diagonal-unit+ 0.70710677
  :test #'=)

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

;;; Movement

(defun direction-velocity (direction)
  (cond
    ((eql direction 'south)
     (vec2 0 -1))
    ((eql direction 'southwest)
     (vec2 (- +diagonal-unit+) (- +diagonal-unit+)))
    ((eql direction 'southeast)
     (vec2 +diagonal-unit+ (- +diagonal-unit+)))
    ((eql direction 'north)
     (vec2 0 1))
    ((eql direction 'northwest)
     (vec2 (- +diagonal-unit+) +diagonal-unit+))
    ((eql direction 'northeast)
     (vec2 +diagonal-unit+ +diagonal-unit+))
    ((eql direction 'east)
     (vec2 1 0))
    ((eql direction 'west)
     (vec2 -1 0))))

(defgeneric move (movable delta-time))

(defclass movable (positionable)
  ((velocity :initform (vec2 0 0) :accessor velocity-of)
   (direction :initform 'south :accessor direction-of)
   (moving-p :initform nil :accessor moving-p)))

(defmethod move ((this movable) delta-time)
  (with-slots (direction velocity position) this
    (let* ((direction-speed (direction-velocity direction))
	   (real-speed (mult
			(mult velocity delta-time)
			direction-speed))
	   (floor-or-ceiling-x
	    (if (> (x direction-speed) 0)
		#'ceiling
		#'floor))
	   (floor-or-ceiling-y
	    (if (> (y direction-speed) 0)
		#'ceiling
		#'floor))
	   (real-position (add position real-speed)))
      (setf position (vec2
		      (funcall floor-or-ceiling-x (x real-position))
		      (funcall floor-or-ceiling-y (y real-position)))))))
