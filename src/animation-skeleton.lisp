;;;; animation-skeleton.lisp
(in-package :deserted)

(define-image 'skeleton-spear "images/skeleton-spear.png")

(defparameter *skeleton-buried*
  (make-animation '(((* 5 64) (* 0 64) (* 6 64) (* 1 64) 0 nil nil))
                  0 :looped-p nil))

(defparameter *skeleton-rise*
  (make-animation '(((* 5 64) (* 0 64) (* 6 64) (* 1 64) 0
                     nil nil)
                    ((* 4 64) (* 0 64) (* 5 64) (* 1 64) 0.125
                     nil nil)
                    ((* 3 64) (* 0 64) (* 4 64) (* 1 64) 0.25
                     nil nil)
                    ((* 2 64) (* 0 64) (* 3 64) (* 1 64) 0.375
                     nil nil)
                    ((* 1 64) (* 0 64) (* 2 64) (* 1 64) 0.5
                     nil nil)
                    ((* 0 64) (* 0 64) (* 1 64) (* 1 64) 0.625
                     nil nil))
                  0.75 :looped-p nil))

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

(defparameter *skeleton-attacking-east*
  (make-animation '(((* 0 64) (* 13 64) (* 1 64) (* 14 64) 0
                     nil nil)
                    ((* 1 64) (* 13 64) (* 2 64) (* 14 64) 0.125
                     nil nil)
                    ((* 2 64) (* 13 64) (* 3 64) (* 14 64) 0.25
                     nil nil)
                    ((* 3 64) (* 13 64) (* 4 64) (* 14 64) 0.375
                     nil nil)
                    ((* 4 64) (* 13 64) (* 5 64) (* 14 64) 0.5
                     nil nil)
                    ((* 5 64) (* 13 64) (* 6 64) (* 14 64) 0.625
                     nil nil)
                    ((* 6 64) (* 13 64) (* 7 64) (* 14 64) 0.75
                     nil nil)
                    ((* 7 64) (* 13 64) (* 8 64) (* 14 64) 0.875
                     nil nil))
                  1 :looped-p nil))

(defparameter *skeleton-attacking-south*
  (make-animation '(((* 0 64) (* 14 64) (* 1 64) (* 15 64) 0
                     nil nil)
                    ((* 1 64) (* 14 64) (* 2 64) (* 15 64) 0.125
                     nil nil)
                    ((* 2 64) (* 14 64) (* 3 64) (* 15 64) 0.25
                     nil nil)
                    ((* 3 64) (* 14 64) (* 4 64) (* 15 64) 0.375
                     nil nil)
                    ((* 4 64) (* 14 64) (* 5 64) (* 15 64) 0.5
                     nil nil)
                    ((* 5 64) (* 14 64) (* 6 64) (* 15 64) 0.625
                     nil nil)
                    ((* 6 64) (* 14 64) (* 7 64) (* 15 64) 0.75
                     nil nil)
                    ((* 7 64) (* 14 64) (* 8 64) (* 15 64) 0.875
                     nil nil))
                  1 :looped-p nil))

(defparameter *skeleton-attacking-west*
  (make-animation '(((* 0 64) (* 15 64) (* 1 64) (* 16 64) 0
                     nil nil)
                    ((* 1 64) (* 15 64) (* 2 64) (* 16 64) 0.125
                     nil nil)
                    ((* 2 64) (* 15 64) (* 3 64) (* 16 64) 0.25
                     nil nil)
                    ((* 3 64) (* 15 64) (* 4 64) (* 16 64) 0.375
                     nil nil)
                    ((* 4 64) (* 15 64) (* 5 64) (* 16 64) 0.5
                     nil nil)
                    ((* 5 64) (* 15 64) (* 6 64) (* 16 64) 0.625
                     nil nil)
                    ((* 6 64) (* 15 64) (* 7 64) (* 16 64) 0.75
                     nil nil)
                    ((* 7 64) (* 15 64) (* 8 64) (* 16 64) 0.875
                     nil nil))
                  1 :looped-p nil))

(defparameter *skeleton-attacking-north*
  (make-animation '(((* 0 64) (* 16 64) (* 1 64) (* 17 64) 0
                     nil nil)
                    ((* 1 64) (* 16 64) (* 2 64) (* 17 64) 0.125
                     nil nil)
                    ((* 2 64) (* 16 64) (* 3 64) (* 17 64) 0.25
                     nil nil)
                    ((* 3 64) (* 16 64) (* 4 64) (* 17 64) 0.375
                     nil nil)
                    ((* 4 64) (* 16 64) (* 5 64) (* 17 64) 0.5
                     nil nil)
                    ((* 5 64) (* 16 64) (* 6 64) (* 17 64) 0.625
                     nil nil)
                    ((* 6 64) (* 16 64) (* 7 64) (* 17 64) 0.75
                     nil nil)
                    ((* 7 64) (* 16 64) (* 8 64) (* 17 64) 0.875
                     nil nil))
                  1 :looped-p nil))

(defparameter *skeleton-dead*
  (make-animation '(((* 0 64) (* 0 64) (* 1 64) (* 1 64) 0
                     nil nil)
                    ((* 1 64) (* 0 64) (* 2 64) (* 1 64) 0.125
                     nil nil)
                    ((* 2 64) (* 0 64) (* 3 64) (* 1 64) 0.25
                     nil nil)
                    ((* 3 64) (* 0 64) (* 4 64) (* 1 64) 0.375
                     nil nil)
                    ((* 4 64) (* 0 64) (* 5 64) (* 1 64) 0.5
                     nil nil))
                    0.675 :looped-p nil))


(defun resolve-skeleton-moving-animation (direction)
  (cond
    ((eql direction 'south)
	   *skeleton-walking-south*)
	  ((eql direction 'north)
	   *skeleton-walking-north*)
	  ((eql direction 'west)
	   *skeleton-walking-west*)
	  ((eql direction 'east)
	   *skeleton-walking-east*)
	  ((eql direction 'southwest)
	   *skeleton-walking-west*)
	  ((eql direction 'southeast)
	   *skeleton-walking-east*)
	  ((eql direction 'northwest)
	   *skeleton-walking-west*)
	  ((eql direction 'northeast)
	   *skeleton-walking-east*)))

(defun resolve-skeleton-attacking-animation (direction)
  (cond
    ((eql direction 'south)
	   *skeleton-attacking-south*)
	  ((eql direction 'north)
	   *skeleton-attacking-north*)
	  ((eql direction 'west)
	   *skeleton-attacking-west*)
	  ((eql direction 'east)
	   *skeleton-attacking-east*)
	  ((eql direction 'southwest)
	   *skeleton-attacking-west*)
	  ((eql direction 'southeast)
	   *skeleton-attacking-east*)
	  ((eql direction 'northwest)
	   *skeleton-attacking-west*)
	  ((eql direction 'northeast)
	   *skeleton-attacking-east*)))
