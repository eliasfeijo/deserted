;;;; skeleton-spear.lisp
(in-package :deserted)

(defclass skeleton-spear (positionable renderable) ())

(defmethod render ((this skeleton-spear))
  (with-slots (position) this
    (let* ((anim *skeleton-walking-east*)
           (frame (get-frame anim (real-time-seconds)))
           (origin (keyframe-origin frame))
           (end (keyframe-end frame)))
      (draw-image position 'skeleton-spear
                  :origin origin
                  :width (- (x end) (x origin))
                  :height (- (y end) (y origin))))))
    
