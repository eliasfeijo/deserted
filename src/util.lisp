;;;; util.lisp
(cl:in-package :deserted)

(defparameter *viewport-width* 800)
(defparameter *viewport-height* 600)
(defparameter *viewport-origin* (vec2 0 0))

(defparameter *white* (vec4 1 1 1 1))
(defparameter *black* (vec4 0 0 0 1))

(defparameter *assets-path* (asdf:system-relative-pathname :deserted "assets/"))

(defparameter *dev-mode* nil)

(defmacro with-dev-mode (&body body)
  `(if *dev-mode*
       (progn
         ,@body)))

(alexandria:define-constant +diagonal-unit+ 0.70710677
  :test #'=)

;; Elapsed time in seconds
(defun real-time-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))

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

;;; Keyboard

(defclass keyboard ()
  ((pressed-keys :initform nil :reader key-combination-of)
   (state-listener :initarg :on-state-change)))

(defun %invoke-state-listener (keyboard)
  (with-slots (state-listener) keyboard
    (when state-listener
      (funcall state-listener keyboard))))

(defmethod press-key ((keyboard keyboard) key)
  (with-slots (pressed-keys state-listener) keyboard
    (push key pressed-keys)
    (%invoke-state-listener keyboard)))

(defmethod release-key ((keyboard keyboard) key)
  (with-slots (pressed-keys) keyboard
    (alexandria:deletef pressed-keys key)
    (%invoke-state-listener keyboard)))

(defun key-combination-pressed-p (keyboard &rest keys)
  (with-slots (pressed-keys) keyboard
    (alexandria:set-equal (intersection pressed-keys keys) keys)))


;;; Find direction of target

(defun target-direction (origin target)
  (cond
    ;; Northeast
    ((and (> (x target) (x origin))
          (> (y target) (y origin)))
     'northeast)
    ;; Northwest
    ((and (< (x target) (x origin))
          (> (y target) (y origin)))
     'northwest)
    ;; Southeast
    ((and (> (x target) (x origin))
          (< (y target) (y origin)))
     'southeast)
    ;; Southwest
    ((and (< (x target) (x origin))
          (< (y target) (y origin)))
     'southwest)
    ;; East
    ((> (x target) (x origin))
     'east)
    ;; West
    ((< (x target) (x origin))
     'west)
    ;; North
    ((> (y target) (y origin))
     'north)
    ;; South
    ((< (y target) (y origin))
     'south)
    (t 'north)))
