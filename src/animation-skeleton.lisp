;;;; animation-skeleton.lisp
(in-package :deserted)

(define-image 'skeleton-spear "images/skeleton-spear.png")

(defparameter *skeleton-buried*
  (make-animation '(((* 5 64) (* 0 64) (* 6 64) (* 1 64) 0 nil nil))
                  0 :looped-p nil))

(defparameter *skeleton-walking-east*
  (make-animation '(((* 0 64) (* 9 64) (* 1 64) (* 10 64) 0
                     nil nil)
                    ((* 1 64) (* 9 64) (* 2 64) (* 10 64) 0.125
                     nil nil)
                    ((* 2 64) (* 9 64) (* 3 64) (* 10 64) 0.25
                     nil nil)
                    ((* 3 64) (* 9 64) (* 4 64) (* 10 64) 0.375
                     nil nil)
                    ((* 4 64) (* 9 64) (* 5 64) (* 10 64) 0.5
                     nil nil)
                    ((* 5 64) (* 9 64) (* 6 64) (* 10 64) 0.625
                     nil nil)
                    ((* 6 64) (* 9 64) (* 7 64) (* 10 64) 0.75
                     nil nil)
                    ((* 7 64) (* 9 64) (* 8 64) (* 10 64) 0.875
                     nil nil)
                    ((* 8 64) (* 9 64) (* 9 64) (* 10 64) 1.0
                     nil nil))
                  1.125 :looped-p t))

(defparameter *skeleton-walking-south*
  (make-animation '(((* 0 64) (* 10 64) (* 1 64) (* 11 64) 0
                     nil nil)
                    ((* 1 64) (* 10 64) (* 2 64) (* 11 64) 0.125
                     nil nil)
                    ((* 2 64) (* 10 64) (* 3 64) (* 11 64) 0.25
                     nil nil)
                    ((* 3 64) (* 10 64) (* 4 64) (* 11 64) 0.375
                     nil nil)
                    ((* 4 64) (* 10 64) (* 5 64) (* 11 64) 0.5
                     nil nil)
                    ((* 5 64) (* 10 64) (* 6 64) (* 11 64) 0.625
                     nil nil)
                    ((* 6 64) (* 10 64) (* 7 64) (* 11 64) 0.75
                     nil nil)
                    ((* 7 64) (* 10 64) (* 8 64) (* 11 64) 0.875
                     nil nil)
                    ((* 8 64) (* 10 64) (* 9 64) (* 11 64) 1.0
                     nil nil))
                  1.125 :looped-p t))

(defparameter *skeleton-walking-west*
  (make-animation '(((* 0 64) (* 11 64) (* 1 64) (* 12 64) 0
                     nil nil)
                    ((* 1 64) (* 11 64) (* 2 64) (* 12 64) 0.125
                     nil nil)
                    ((* 2 64) (* 11 64) (* 3 64) (* 12 64) 0.25
                     nil nil)
                    ((* 3 64) (* 11 64) (* 4 64) (* 12 64) 0.375
                     nil nil)
                    ((* 4 64) (* 11 64) (* 5 64) (* 12 64) 0.5
                     nil nil)
                    ((* 5 64) (* 11 64) (* 6 64) (* 12 64) 0.625
                     nil nil)
                    ((* 6 64) (* 11 64) (* 7 64) (* 12 64) 0.75
                     nil nil)
                    ((* 7 64) (* 11 64) (* 8 64) (* 12 64) 0.875
                     nil nil)
                    ((* 8 64) (* 11 64) (* 9 64) (* 12 64) 1.0
                     nil nil))
                  1.125 :looped-p t))

(defparameter *skeleton-walking-north*
  (make-animation '(((* 0 64) (* 12 64) (* 1 64) (* 13 64) 0
                     nil nil)
                    ((* 1 64) (* 12 64) (* 2 64) (* 13 64) 0.125
                     nil nil)
                    ((* 2 64) (* 12 64) (* 3 64) (* 13 64) 0.25
                     nil nil)
                    ((* 3 64) (* 12 64) (* 4 64) (* 13 64) 0.375
                     nil nil)
                    ((* 4 64) (* 12 64) (* 5 64) (* 13 64) 0.5
                     nil nil)
                    ((* 5 64) (* 12 64) (* 6 64) (* 13 64) 0.625
                     nil nil)
                    ((* 6 64) (* 12 64) (* 7 64) (* 13 64) 0.75
                     nil nil)
                    ((* 7 64) (* 12 64) (* 8 64) (* 13 64) 0.875
                     nil nil)
                    ((* 8 64) (* 12 64) (* 9 64) (* 13 64) 1.0
                     nil nil))
                  1.125 :looped-p t))
