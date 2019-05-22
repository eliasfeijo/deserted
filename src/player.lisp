;;;; player.lisp
(cl:in-package :deserted)

(defconstant +player-max-hp+ 100)

(defclass player (renderable movable)
  ((size :initform (vec2 64 64) :reader size-of)
   (velocity :initform (vec2 100 100))
   (state :initform 'idle :reader state-of)
   (state-started :initform (real-time-seconds))
   (current-animation :initform *player-idle-south*)
   (hp :initform +player-max-hp+)))

(defun update-player (player world delta-time)
  (with-slots (state current-animation) player
    (unless (eql state 'dead)
      (cond
        ((eql state 'moving)
         (move player delta-time))
        ((eql state 'attacking)
         (when (animation-finished-p
                current-animation (real-time-seconds))
           (play-sound 'punch)
           (set-state player 'idle)
           (loop for enemy across (enemies-of world) do
                (if (intersect-p
                     (vec4
                      (x (position-of enemy))
                      (y (position-of enemy))
                      (x (size-of enemy))
                      (y (size-of enemy)))
                     (resolve-player-attack-rect player))
                    (kill-enemy enemy)))))))))

(defun take-damage-player (player damage)
  (with-slots (hp state) player
    (unless (eql state 'dead)
      (setf hp (- hp damage))
      (if (<= hp 0)
          (progn
            (set-state player 'dead))))))

(defun resolve-player-attack-rect (player)
  (with-slots (position size direction) player
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
          (- (y center) 35)
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
          (- (x center) 15)
          (+ (y center) 5)
          30
          30))
        ((eql direction 'east)
         (vec4
          (+ (x center) 5)
          (- (y center) 20)
          30
          30))
        ((eql direction 'west)
         (vec4
          (- (x center) 35)
          (- (y center) 20)
          25
          30))
        ((eql direction 'south)
         (vec4
          (- (x center) 15)
          (- (y center) 35)
          30
          30))
        (t
         (vec4
          (- (x center) 20)
          (- (y center) 35)
          40
          30))))))

(defun set-state (player new-state)
  (with-slots (state state-started current-animation
                     direction)
      player
    (setf state new-state
          state-started (real-time-seconds)
          current-animation (resolve-player-animation state direction))
    (if (or (eql state 'attacking) (eql state 'dead))
        (start-animation current-animation state-started))))

(defun resolve-player-animation (state direction)
  (cond
    ((eql state 'idle)
     (resolve-player-idle-animation direction))
    ((eql state 'moving)
     (resolve-player-moving-animation direction))
    ((eql state 'attacking)
     (resolve-player-attacking-animation direction))
    ((eql state 'dead) *player-dead*)))
      

(defmethod render ((this player))
  (with-slots (position direction size moving-p
                        current-animation state hp)
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
        (draw-rect (vec2
                    (x real-position)
                    (- (y real-position) 10))
                   (x size) 10
                   :fill-paint (vec4 1 0 0 1))
        (draw-rect (vec2
                    (x real-position)
                    (- (y real-position) 10))
                   (max 0 (* (x size) (/ hp +player-max-hp+))) 10
                   :fill-paint (vec4 0 1 0 1))
        (with-dev-mode
          (if (eql state 'attacking)
              (let ((attack-rect (resolve-player-attack-rect this)))
                (draw-rect (vec2 (x attack-rect)
                                 (y attack-rect))
                           (z attack-rect) (w attack-rect)
                           :fill-paint (vec4 1 0 0 0.5))))
          (draw-rect real-position (x size) (y size) :stroke-paint *black*))))))
