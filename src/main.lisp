;;;; main.lisp
(cl:in-package :deserted)

(defgame deserted ()
  ()
  (:viewport-width 800)
  (:viewport-height 600)
  (:viewport-title "Deserted"))

(defun play (&optional blocking)
  (gamekit:start 'deserted :blocking blocking))
