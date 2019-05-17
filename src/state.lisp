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
   (camera)))

(defmethod initialize-instance :after ((this game) &key)
  (with-slots (world camera) this
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
    (update-camera camera)))
      

(defmethod render ((this game))
  (with-slots (world camera) this
    (with-pushed-canvas ()
      (translate-canvas (- (x (offset-of camera)))
			(- (y (offset-of camera))))
      (render world))))


