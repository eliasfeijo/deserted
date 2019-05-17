;;;; main.lisp
(cl:in-package :deserted)

(defgame deserted ()
  ()
  (:viewport-width *viewport-width*)
  (:viewport-height *viewport-height*)
  (:viewport-title "Deserted"))

(defun play (&optional blocking)
  (gamekit:start 'deserted :blocking blocking))
