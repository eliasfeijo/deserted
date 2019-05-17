;;;; main.lisp
(cl:in-package :deserted)

(register-resource-package :deserted *assets-path*)

(defgame deserted ()
  ((game-state))
  (:viewport-width *viewport-width*)
  (:viewport-height *viewport-height*)
  (:viewport-title "Deserted")
  (:prepare-resources nil))

(defmethod post-initialize :after ((this deserted))
  (with-slots (game-state) this
    (setf game-state (make-instance 'resource-preparation))))

(defmethod draw ((this deserted))
  (with-slots (game-state) this
    (render game-state)))

(defun play (&optional blocking)
  (gamekit:start 'deserted :blocking blocking))
