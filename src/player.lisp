;;;; player.lisp
(cl:in-package :deserted)

(defclass player (renderable movable)
  ((size :initform (vec2 32 64) :reader size-of)
   (velocity :initform (vec2 100 100))))
