;;;; animation-player.lisp
(in-package :deserted)

(define-image 'pirate "images/pirate.png")

(defparameter *player-walking-east*
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

(defparameter *player-walking-south*
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

(defparameter *player-walking-west*
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

(defparameter *player-walking-north*
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

(defparameter *player-attacking-east*
  (make-animation '(((* 0 64) (* 5 64) (* 1 64) (* 6 64) 0
                     nil nil)
                    ((* 1 64) (* 5 64) (* 2 64) (* 6 64) 0.125
                     nil nil)
                    ((* 2 64) (* 5 64) (* 3 64) (* 6 64) 0.25
                     nil nil)
                    ((* 3 64) (* 5 64) (* 4 64) (* 6 64) 0.375
                     nil nil)
                    ((* 4 64) (* 5 64) (* 5 64) (* 6 64) 0.5
                     nil nil)
                    ((* 5 64) (* 5 64) (* 6 64) (* 6 64) 0.625
                     nil nil))
                  0.75 :looped-p nil))

(defparameter *player-attacking-south*
  (make-animation '(((* 0 64) (* 6 64) (* 1 64) (* 7 64) 0
                     nil nil)
                    ((* 1 64) (* 6 64) (* 2 64) (* 7 64) 0.125
                     nil nil)
                    ((* 2 64) (* 6 64) (* 3 64) (* 7 64) 0.25
                     nil nil)
                    ((* 3 64) (* 6 64) (* 4 64) (* 7 64) 0.375
                     nil nil)
                    ((* 4 64) (* 6 64) (* 5 64) (* 7 64) 0.5
                     nil nil)
                    ((* 5 64) (* 6 64) (* 6 64) (* 7 64) 0.625
                     nil nil))
                  0.75 :looped-p nil))

(defparameter *player-attacking-west*
  (make-animation '(((* 0 64) (* 7 64) (* 1 64) (* 8 64) 0
                     nil nil)
                    ((* 1 64) (* 7 64) (* 2 64) (* 8 64) 0.125
                     nil nil)
                    ((* 2 64) (* 7 64) (* 3 64) (* 8 64) 0.25
                     nil nil)
                    ((* 3 64) (* 7 64) (* 4 64) (* 8 64) 0.375
                     nil nil)
                    ((* 4 64) (* 7 64) (* 5 64) (* 8 64) 0.5
                     nil nil)
                    ((* 5 64) (* 7 64) (* 6 64) (* 8 64) 0.625
                     nil nil))
                  0.75 :looped-p nil))

(defparameter *player-attacking-north*
  (make-animation '(((* 0 64) (* 8 64) (* 1 64) (* 9 64) 0
                     nil nil)
                    ((* 1 64) (* 8 64) (* 2 64) (* 9 64) 0.125
                     nil nil)
                    ((* 2 64) (* 8 64) (* 3 64) (* 9 64) 0.25
                     nil nil)
                    ((* 3 64) (* 8 64) (* 4 64) (* 9 64) 0.375
                     nil nil)
                    ((* 4 64) (* 8 64) (* 5 64) (* 9 64) 0.5
                     nil nil)
                    ((* 5 64) (* 8 64) (* 6 64) (* 9 64) 0.625
                     nil nil))
                    0.75 :looped-p nil))
  
(defun resolve-player-moving-animation (direction)
  (cond
    ((eql direction 'south)
	   *player-walking-south*)
	  ((eql direction 'north)
	   *player-walking-north*)
	  ((eql direction 'west)
	   *player-walking-west*)
	  ((eql direction 'east)
	   *player-walking-east*)
	  ((eql direction 'southwest)
	   *player-walking-west*)
	  ((eql direction 'southeast)
	   *player-walking-east*)
	  ((eql direction 'northwest)
	   *player-walking-west*)
	  ((eql direction 'northeast)
	   *player-walking-east*)))

(defun resolve-player-attacking-animation (direction)
  (cond
    ((eql direction 'south)
	   *player-attacking-south*)
	  ((eql direction 'north)
	   *player-attacking-north*)
	  ((eql direction 'west)
	   *player-attacking-west*)
	  ((eql direction 'east)
	   *player-attacking-east*)
	  ((eql direction 'southwest)
	   *player-attacking-west*)
	  ((eql direction 'southeast)
	   *player-attacking-east*)
	  ((eql direction 'northwest)
	   *player-attacking-west*)
	  ((eql direction 'northeast)
	   *player-attacking-east*)))
