;;;; player.lisp
(cl:in-package :deserted)

(defclass player (renderable movable)
  ((size :initform (vec2 64 64) :reader size-of)
   (velocity :initform (vec2 100 100))
   (state :initform 'idle :reader state-of)
   (state-started :initform (real-time-seconds))
   (current-animation :initform *player-idle-south*)))

(defun update-player (player delta-time)
  (with-slots (state current-animation) player
    (cond
      ((eql state 'moving)
       (move player delta-time))
      ((eql state 'attacking)
       (when (animation-finished-p
              current-animation (real-time-seconds))
         (set-state player 'idle))))))
  

(defun set-state (player new-state)
  (with-slots (state state-started current-animation
                     direction)
      player
    (setf state new-state
          state-started (real-time-seconds)
          current-animation (resolve-player-animation state direction))
    (if (eql state 'attacking)
        (start-animation current-animation state-started))))

(defun resolve-player-animation (state direction)
  (cond
    ((eql state 'idle)
     (resolve-player-idle-animation direction))
    ((eql state 'moving)
     (resolve-player-moving-animation direction))
    ((eql state 'attacking)
     (resolve-player-attacking-animation direction))))
      

(defmethod render ((this player))
  (with-slots (position direction size moving-p
                        current-animation)
      this
    (let* ((frame (get-frame current-animation (real-time-seconds)))
           (origin (keyframe-origin frame))
           (end (keyframe-end frame))
           (flipped-x (keyframe-flipped-x frame))
           (flipped-y (keyframe-flipped-y frame))
           (real-position position))
      (with-pushed-canvas ()
        (if (and flipped-x flipped-y)
            (progn
              (setf real-position
                    (vec2
                     (+ (- (x position)) (- (x size)))
                     (+ (- (y position)) (- (y size)))))
              (scale-canvas -1 -1))
            (if flipped-x
                (progn
                  (setf real-position
                        (vec2
                         (+ (- (x position)) (- (x size)))
                         (y position)))
                  (scale-canvas -1 1))
                (if flipped-y
                    (progn 
                      (setf real-position
                            (vec2
                             (x position)
                             (+ (- (y position)) (- (y size)))))
                      (scale-canvas 1 -1)))))
        (draw-image real-position
                    'pirate
                    :origin origin
                    :width (- (x end) (x origin))
                    :height (- (y end) (y origin)))
        (with-dev-mode
          (draw-rect real-position (x size) (y size) :stroke-paint *black*))))))
