;;;; animation.lisp
(in-package :deserted)

(defstruct keyframe
  (origin (vec2 0 0) :type vec2 :read-only t)
  (end (vec2 0 0) :type vec2 :read-only t)
  (time 0 :type single-float :read-only t)
  (flipped-x nil :type boolean :read-only t)
  (flipped-y nil :type boolean :read-only t))

(defclass animation ()
  ((sequence :initarg :sequence :initform nil)
   (looped-p :initarg :looped-p :initform nil)
   (started :initform 0 :reader start-time-of)
   (animation-length :initarg :animation-length :initform 0 :reader animation-length-of)))

(defun make-animation (sequence animation-length &key looped-p)
  (let ((frames (loop for (x-orig y-orig x-end y-end time flipped-x flipped-y) in (sort sequence #'< :key #'fifth)
                   collect (make-keyframe :origin (vec2 (eval x-orig) (eval y-orig))
                                          :end (vec2 (eval x-end) (eval y-end))
                                          :time (bodge-util:f time)
					  :flipped-x flipped-x
					  :flipped-y flipped-y))))
    (make-instance 'animation
                   :looped-p looped-p
		   :animation-length animation-length
                   :sequence (make-array (length frames)
                                         :element-type 'keyframe
                                         :initial-contents frames))))

(defun start-animation (animation current-time)
  (with-slots (started) animation
    (setf started current-time)))

(defun get-looped-time (animation time)
  (let ((animation-length (animation-length-of animation)))
    (if (= animation-length 0)
        0
        (mod time animation-length))))

(defun get-frame (animation current-time)
  (with-slots (sequence started looped-p) animation
    (let* ((time-delta (- current-time started))
           (animation-timestamp (if looped-p (get-looped-time animation time-delta) time-delta)))
      (multiple-value-bind (result idx)
          (bodge-util:search-sorted animation-timestamp sequence :test #'= :key #'keyframe-time)
        (or result (aref sequence (max (1- idx) 0)))))))
