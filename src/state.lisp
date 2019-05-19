;;;; state.lisp
(cl:in-package :deserted)

(defclass game-state () ())

(defclass resource-preparation (game-state) ())

(defmethod render ((this resource-preparation))
  (draw-rect *viewport-origin* *viewport-width* *viewport-height* :fill-paint *black*)
  (draw-text "Deserted" (vec2 370 400) :fill-color *white*)
  (draw-text "Loading..." (vec2 370 350) :fill-color *white*))

(defclass game (game-state)
  ((world :initarg :world)
   (camera)
   (keyboard)
   (fog :initform (make-instance 'fog))
   (last-updated :initform (real-time-seconds))))

(defmethod press-key ((this game) key)
  (with-slots (keyboard) this
    (if (eql key :f5)
        (setf *dev-mode* (not *dev-mode*)))
    (press-key keyboard key)))

(defmethod release-key ((this game) key)
  (with-slots (keyboard) this
    (release-key keyboard key)))

(defun process-movement-input (keyboard)
  (cond
    ((key-combination-pressed-p keyboard :w :d)
     'northeast)
    ((key-combination-pressed-p keyboard :s :d)
     'southeast)
    ((key-combination-pressed-p keyboard :a :s)
     'southwest)
    ((key-combination-pressed-p keyboard :a :w)
     'northwest)
    ((key-combination-pressed-p keyboard :w)
     'north)
    ((key-combination-pressed-p keyboard :a)
     'west)
    ((key-combination-pressed-p keyboard :s)
     'south)
    ((key-combination-pressed-p keyboard :d)
     'east)))

(defmethod initialize-instance :after ((this game) &key)
  (with-slots (world camera keyboard) this
    (let ((real-map-width (*
                           (width-of (map-of world))
                           (tile-width-of (map-of world))))
          (real-map-height (*
                            (height-of (map-of world))
                            (tile-height-of (map-of world)))))
      (setf camera (make-instance 'camera
                                  :target (player-of world)
                                  :map-width real-map-width
                                  :map-height real-map-height)))
    (update-camera camera)
    (labels
        ((update-moving-p-and-direction (keyboard)
           (let ((result (process-movement-input keyboard)))
             (if (null result)
                 (setf (moving-p (player-of world)) nil)
                 (setf (moving-p (player-of world)) t
                       (direction-of (player-of world)) result)))))
      (setf keyboard (make-instance 'keyboard :on-state-change #'update-moving-p-and-direction)))))

(defmethod act ((this game))
  (with-slots (world last-updated camera fog) this
    (let* ((current-time (real-time-seconds))
           (delta-time (- current-time last-updated)))
      (if (moving-p (player-of world))
          (move (player-of world) delta-time))
      (loop for enemy across (enemies-of world) do
           (update-skeleton-spear enemy world))
      (update-camera camera)
      (update-fog fog delta-time)
      (setf last-updated current-time))))

(defmethod render ((this game))
  (with-slots (world camera fog) this
    (with-pushed-canvas ()
      (translate-canvas (- (x (offset-of camera)))
                        (- (y (offset-of camera))))
      (render world))
    (render fog)
    (with-dev-mode
      (draw-line (vec2 (/ *viewport-width* 2) 0) (vec2 (/ *viewport-width* 2) *viewport-height*) (vec4 0 1 0 1))
      (draw-line (vec2 0 (/ *viewport-height* 2)) (vec2 *viewport-width* (/ *viewport-height* 2)) (vec4 0 1 0 1))
      (let* ((player-position-text
              (format nil "x: ~d y: ~d"
                      (x (position-of (player-of world)))
                      (y (position-of (player-of world)))))
             (player-map-position
              (div
               (position-of (player-of world))
               (vec2 (tile-width-of (map-of world))
                     (tile-height-of (map-of world)))))
             (player-map-position-text
              (format nil "x: ~d y: ~d"
                      (ceiling (x player-map-position))
                      (ceiling (y player-map-position)))))
        (draw-text player-position-text (vec2 10 (- *viewport-height* 20)) :fill-color *black*)
        (draw-text player-map-position-text (vec2 10 (- *viewport-height* 50)) :fill-color *black*)))))


