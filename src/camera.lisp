;;;; camera.lisp
(cl:in-package :deserted)

(defgeneric (setf offset-of) (vec2 camera))

(defgeneric update-camera (camera))

(defclass camera ()
  ((map-width :initarg :map-width)
   (map-height :initarg :map-height)
   (target :initarg :target :accessor target-of)
   (offset :initform (vec2 0 0) :reader offset-of)))

(defmethod (setf offset-of) ((vec vec2) (this camera))
  (setf (x (offset-of this)) (x vec))
	(y (offset-of this)) (y vec))

(defmethod update-camera ((this camera))
  (with-slots (target offset map-width map-height) this
    (let* ((camera-vertical-extent (/ *viewport-height* 2))
           (camera-horizontal-extent (/ *viewport-width* 2))
           (bounds-min *viewport-origin*)
           (bounds-max (vec2 map-width map-height))
           (left-bound (+ (x bounds-min) camera-horizontal-extent))
           (right-bound (- (x bounds-max) camera-horizontal-extent))
           (bottom-bound (+ (y bounds-min) camera-vertical-extent))
           (top-bound (- (y bounds-max) camera-vertical-extent))
           (center-of-target (add (position-of target) (div (size-of target) 2)))
           (cam-x (alexandria:clamp (x center-of-target) left-bound right-bound))
           (cam-y (alexandria:clamp (y center-of-target) bottom-bound top-bound)))
      (setf offset (vec2 (-
                          cam-x camera-horizontal-extent)
                         (-
                          cam-y camera-vertical-extent))))))
