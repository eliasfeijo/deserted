;;;; skeleton-spear.lisp
(in-package :deserted)

(defclass skeleton-spear (movable renderable)
  ((velocity :initform (vec2 100 100))
   (size :initform (vec2 64 64))
   (vision-area :initform 50)
   (aggro :initform nil)))

(defun update-skeleton-spear (skeleton world)
  (with-slots (position size vision-area aggro) skeleton
    (let ((vision-rect (vec4
                        (- (x position) vision-area)
                        (- (y position) vision-area)
                        (+ (* 2 vision-area) (x size))
                        (+ (* 2 vision-area) (y size))))
          (player-rect (vec4
                        (x (position-of (player-of world)))
                        (y (position-of (player-of world)))
                        (x (size-of (player-of world)))
                        (y (size-of (player-of world))))))
      (if (intersect-p vision-rect player-rect)
          (setf aggro t)))))

(defun intersect-p (rect1 rect2)
  (if (and
       (> (+ (x rect1) (z rect1)) (x rect2))
       (< (x rect1) (+ (x rect2) (z rect2))))
      (if (and
           (> (+ (y rect1) (w rect1)) (y rect2))
           (< (y rect1) (+ (y rect2) (w rect2))))
          t)))

(defmethod render ((this skeleton-spear))
  (with-slots (position size vision-area aggro) this
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
                      (- (x position) vision-area)
                      (- (y position) vision-area))
                     (+ (* 2 vision-area) (x size))
                     (+ (* 2 vision-area) (y size))
                     :fill-paint fill-color))))))
    
