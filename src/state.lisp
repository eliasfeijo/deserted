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
   (player-initial-position :initarg :player-initial-position)))

(defmethod render ((this game))
  (with-slots (world) this
    (render world)))


