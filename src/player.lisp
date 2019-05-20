;;;; player.lisp
(cl:in-package :deserted)

(defclass player (renderable movable)
  ((size :initform (vec2 64 64) :reader size-of)
   (velocity :initform (vec2 100 100))))

(defmethod render ((this player))
  (with-slots (position direction size moving-p) this
    (let* ((anim (resolve-player-moving-animation direction))
	   (frame (get-frame anim (real-time-seconds)))
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
