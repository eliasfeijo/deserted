;;;; state.lisp
(cl:in-package :deserted)

(defclass game-state () ())

(defmethod press-key ((this game-state) key)
  (declare (ignore this key)))

(defmethod release-key ((this game-state) key)
  (declare (ignore this key)))

(defclass resource-preparation (game-state) ())

(defmethod render ((this resource-preparation))
  (draw-rect *viewport-origin* *viewport-width* *viewport-height* :fill-paint *black*)
  (draw-text "Deserted" (vec2 370 400) :fill-color *white*)
  (draw-text "Loading..." (vec2 370 350) :fill-color *white*))

(defclass game (game-state)
  ((world :initarg :world)
   (camera)
   (keyboard)
   (game-over :initform nil)
   (fog :initform (make-instance 'fog))
   (last-updated :initform (real-time-seconds))))

(defmethod press-key ((this game) key)
  (with-slots (keyboard world) this
    (cond
      ((eql key :escape)
       (if (eql (state-of (player-of world)) 'dead)
           (progn
             (clear-world world)
             (clear-game this))))
      ((eql key :f5)
       (setf *dev-mode* (not *dev-mode*)))
      ((eql key :space)
       (if (and (not (eql (state-of (player-of world))
                          'attacking))
                (not (eql (state-of (player-of world))
                          'dead)))
           (set-state (player-of world) 'attacking))))
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
  (clear-game this))

(defun clear-game (game)
  (with-slots (world camera keyboard) game
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
             (if (and (not (eql (state-of (player-of world))
                                'attacking))
                      (not (eql (state-of (player-of world))
                                'dead)))
                 (if (null result)
                     (set-state (player-of world) 'idle)
                     (progn
                       (setf (direction-of (player-of world)) result)
                       (set-state (player-of world) 'moving)))))))
      (setf keyboard (make-instance 'keyboard :on-state-change #'update-moving-p-and-direction)))))

(defmethod act ((this game))
  (with-slots (world last-updated camera fog game-over) this
    (let* ((current-time (real-time-seconds))
           (delta-time (- current-time last-updated)))
      (unless game-over
        (if (not (in-inventory-p (key-of world)))
            (if (intersect-p (vec4
                              (x (position-of (player-of world)))
                              (y (position-of (player-of world)))
                              (x (size-of (player-of world)))
                              (y (size-of (player-of world))))
                             (vec4
                              (x (position-of (key-of world)))
                              (y (position-of (key-of world)))
                              (x (size-of (key-of world)))
                              (y (size-of (key-of world)))))
                (setf (in-inventory-p (key-of world)) t))
            (if (intersect-p (vec4
                              (x (position-of (player-of world)))
                              (y (position-of (player-of world)))
                              (x (size-of (player-of world)))
                              (y (size-of (player-of world))))
                             (vec4
                              (x (position-of (chest-of world)))
                              (y (position-of (chest-of world)))
                              (x (size-of (chest-of world)))
                              (y (size-of (chest-of world)))))
                (progn
                  (stop-sound 'times-of-unrest)
                  (play-sound 'win)
                  (setf game-over t))))
        (update-player (player-of world) world delta-time)
        (loop for enemy across (enemies-of world) do
             (update-skeleton-spear enemy world delta-time))
        (update-camera camera)
        (update-fog fog delta-time)
        (setf last-updated current-time)))))

(defmethod render ((this game))
  (with-slots (world camera fog game-over) this
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
        (draw-text player-map-position-text (vec2 10 (- *viewport-height* 50)) :fill-color *black*)))
    (if game-over
        (progn
          (draw-rect (vec2 0 0) *viewport-width* *viewport-height*
                     :fill-paint *black*)
          (draw-text "Game Over."
                     (vec2 350 400) :fill-color *white*)
          (draw-text "Made by Elias Feijó"
                     (vec2 320 370) :fill-color *white*)))))


