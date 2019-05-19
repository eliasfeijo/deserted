;;;; skeleton-spear.lisp
(in-package :deserted)

(defclass skeleton-spear (movable renderable)
  ((velocity :initform (vec2 100 100))
   (size :initform (vec2 64 64))
   (vision-range :initform 50)
   (aggro :initform nil)))

(defun update-skeleton-spear (skeleton world)
  (with-slots (position size vision-range aggro) skeleton
    (let ((vision-rect (vec4
                        (- (x position) vision-range)
                        (- (y position) vision-range)
                        (+ (* 2 vision-range) (x size))
                        (+ (* 2 vision-range) (y size))))
          (player-rect (vec4
                        (x (position-of (player-of world)))
                        (y (position-of (player-of world)))
                        (x (size-of (player-of world)))
                        (y (size-of (player-of world))))))
      (if (intersect-p vision-rect player-rect)
          (setf aggro t)))))

(defmethod render ((this skeleton-spear))
  (with-slots (position size vision-range aggro) this
    (let* ((anim *skeleton-buried*)
           (frame (get-frame anim (real-time-seconds)))
           (origin (keyframe-origin frame))
           (end (keyframe-end frame)))
      (draw-image position 'skeleton-spear
                  :origin origin
                  :width (- (x end) (x origin))
                  :height (- (y end) (y origin)))
      (with-dev-mode
        (draw-rect position
                   (x size) (y size)
                   :stroke-paint (vec4 1 0 0 1))
        (let ((fill-color
               (if aggro
                   (vec4 1 0 0 0.3)
                   (vec4 0 1 0 0.3))))
          (draw-rect (vec2
                      (- (x position) vision-range)
                      (- (y position) vision-range))
                     (+ (* 2 vision-range) (x size))
                     (+ (* 2 vision-range) (y size))
                     :fill-paint fill-color))))))
    
