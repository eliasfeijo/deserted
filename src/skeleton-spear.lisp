;;;; skeleton-spear.lisp
(in-package :deserted)

(defclass skeleton-spear (movable renderable)
  ((velocity :initform (vec2 100 100))
   (size :initform (vec2 64 64))
   (vision-range :initform 50)
   (aggro :initform nil)
   (state :initform 'buried)
   (state-started :initform (real-time-seconds))
   (current-animation :initform *skeleton-buried*)))

(defun update-skeleton-spear (skeleton world)
  (with-slots (position size vision-range aggro
                        state state-started current-animation)
      skeleton
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
          (setf aggro t))
      (when aggro
        (cond ((eql state 'buried)
                 (setf state 'rising
                     state-started (real-time-seconds)
                     current-animation *skeleton-rise*)
               (start-animation current-animation state-started)))))))

(defmethod render ((this skeleton-spear))
  (with-slots (position size vision-range aggro current-animation) this
    (let* ((frame (get-frame current-animation (real-time-seconds)))
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
    
