;;;; animation-skeleton.lisp
(in-package :deserted)

(define-image 'skeleton-spear "images/skeleton-spear.png")

(defparameter *skeleton-buried*
  (make-animation '(((* 5 64) (* 0 64) (* 6 64) (* 1 64) 0 nil nil))
                  0 :looped-p nil))

(defparameter *skeleton-walking-east*
  (make-animation '(((* 0 64) (* 9 64) (* 1 64) (* 10 64) 0 nil nil)
                    ((* 1 64) (* 9 64) (* 2 64) (* 10 64) 0.1 nil nil)
                    ((* 2 64) (* 9 64) (* 3 64) (* 10 64) 0.2 nil nil)
                    ((* 3 64) (* 9 64) (* 4 64) (* 10 64) 0.3 nil nil)
                    ((* 4 64) (* 9 64) (* 5 64) (* 10 64) 0.4 nil nil)
                    ((* 5 64) (* 9 64) (* 6 64) (* 10 64) 0.5 nil nil)
                    ((* 6 64) (* 9 64) (* 7 64) (* 10 64) 0.6 nil nil)
                    ((* 7 64) (* 9 64) (* 8 64) (* 10 64) 0.7 nil nil)
                    ((* 8 64) (* 9 64) (* 9 64) (* 10 64) 0.8 nil nil))
                  0.8 :looped-p t))
