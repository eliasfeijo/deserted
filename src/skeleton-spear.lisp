;;;; skeleton-spear.lisp
(in-package :deserted)

(defclass skeleton-spear (movable renderable)
  ((velocity :initform (vec2 60 60))
   (size :initform (vec2 64 64) :reader size-of)
   (vision-range :initform 50)
   (aggro :initform nil)
   (state :initform 'buried)
   (state-started :initform (real-time-seconds))
   (current-animation :initform *skeleton-buried*)
   (attack-range :initform 15)))

(defun update-skeleton-spear (skeleton world delta-time)
  (with-slots (position size vision-range aggro direction
                        state state-started current-animation
                        attack-range)
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
        (cond
          ;;; buried -> rising
          ((eql state 'buried)
           (setf state 'rising
                 state-started (real-time-seconds)
                 current-animation *skeleton-rise*)
           (play-sound 'bones)
           (start-animation current-animation state-started))
          ;;; rising -> moving
          ((eql state 'rising)
           (if (animation-finished-p
                current-animation (real-time-seconds))
               (setf state 'moving
                     state-started (real-time-seconds)
                     current-animation (resolve-skeleton-moving-animation direction))))
          ;;; moving -> attacking
          ((eql state 'moving)
           (setf direction (target-direction position (position-of (player-of world)))
                 current-animation (resolve-skeleton-moving-animation direction))
           (move skeleton delta-time)
           (if (< (distance
                   (position-of skeleton)
                   (position-of (player-of world)))
                  attack-range)
               (progn
                 (setf state 'attacking
                       state-started (real-time-seconds)
                       current-animation (resolve-skeleton-attacking-animation direction))
                 (start-animation current-animation state-started))))
          ;;; attacking -> moving or attacking
          ((eql state 'attacking)
           (if (animation-finished-p
                current-animation (real-time-seconds))
               (let* ((player-position (position-of (player-of world)))
                      (player-size (size-of (player-of world)))
                      (player-rect (vec4
                                    (x player-position)
                                    (y player-position)
                                    (x player-size)
                                    (y player-size))))
                 (if (intersect-p
                      (resolve-spear-skeleton-attack-rect skeleton)
                      player-rect)
                     ;; collided
                     (progn
                       (take-damage-player (player-of world)
                                           (max 20 (random 51)))
                       (setf state 'moving
                             state-started (real-time-seconds)
                             current-animation (resolve-skeleton-moving-animation direction)))
                     ;; else
                     (if (< (distance
                             (position-of skeleton)
                             (position-of (player-of world)))
                            attack-range)
                         ;; is inside attack range
                         (progn
                           (setf direction
                                 (target-direction
                                  position
                                  (position-of (player-of world)))
                                 current-animation
                                 (resolve-skeleton-attacking-animation direction))
                           (start-animation current-animation (real-time-seconds)))
                         ;; is outside attack range
                         (progn
                           (setf state 'moving
                                 state-started (real-time-seconds)
                                 current-animation (resolve-skeleton-moving-animation direction)))))))))))))

(defun resolve-spear-skeleton-attack-rect (skeleton)
  (with-slots (position size direction) skeleton
    (let ((center (center-of (vec4
                              (x position) (y position)
                              (x size) (y size)))))
      (cond
        ((eql direction 'northeast)
         (vec4
          (+ (x center) 5)
          (+ (y center) 5)
          30
          30))
        ((eql direction 'northwest)
         (vec4
          (- (x center) 35)
          (+ (y center) 5)
          30
          30))
        ((eql direction 'southeast)
         (vec4
          (+ (x center) 5)
          (- (y center) 30)
          30
          30))
        ((eql direction 'southwest)
         (vec4
          (- (x center) 35)
          (- (y center) 35)
          30
          30))
        ((eql direction 'north)
         (vec4
          (+ (x center) 5)
          (y center)
          15
          30))
        ((eql direction 'east)
         (vec4
          (+ (x center) 5)
          (- (y center) 20)
          30
          15))
        ((eql direction 'west)
         (vec4
          (- (x center) 35)
          (- (y center) 20)
          30
          15))
        ((eql direction 'south)
         (vec4
          (- (x center) 20)
          (- (y center) 35)
          15
          30))
        (t
         (vec4
          (- (x center) 20)
          (- (y center) 35)
          15
          30))))))

(defun kill-enemy (enemy)
  (with-slots (state current-animation) enemy
    (setf state 'dead
          current-animation *skeleton-buried*)))

(defmethod render ((this skeleton-spear))
  (with-slots (position size vision-range aggro current-animation
                        state)
      this
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
                   (vec4 0 1 0 0.3)))
              (attack-rect (resolve-spear-skeleton-attack-rect this)))
          (if (eql state 'attacking)
              (draw-rect (vec2 (x attack-rect) (y attack-rect))
                         (z attack-rect) (w attack-rect)
                         :fill-paint (vec4 1 0 0 0.5)))
          (draw-rect (vec2
                      (- (x position) vision-range)
                      (- (y position) vision-range))
                     (+ (* 2 vision-range) (x size))
                     (+ (* 2 vision-range) (y size))
                     :fill-paint fill-color))))))
    
