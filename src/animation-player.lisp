;;;; animation-player.lisp
(in-package :deserted)

(define-image 'human-male-female "images/human-male-female.png")

;;; Idle animation

(defparameter *human-male-idle-south*
  (make-animation '((0 64 32 128 0 nil nil))
		  0 :looped-p nil))

(defparameter *human-male-idle-north*
  (make-animation '((64 64 96 128 0 nil nil))
		  0 :looped-p nil))

(defparameter *human-male-idle-west*
  (make-animation '((160 64 192 128 0 nil nil))
		  0 :looped-p nil))

(defparameter *human-male-idle-east*
  (make-animation '((160 64 192 128 0 t nil))
		  0 :looped-p nil))

(defparameter *human-male-idle-southwest*
  (make-animation '((256 64 288 128 0.25 nil nil))
		  0 :looped-p nil))

(defparameter *human-male-idle-southeast*
  (make-animation '((256 64 288 128 0.25 t nil))
		  0 :looped-p nil))

(defparameter *human-male-idle-northwest*
  (make-animation '((352 64 384 128 0.25 nil nil))
		  0 :looped-p nil))

(defparameter *human-male-idle-northeast*
  (make-animation '((352 64 384 128 0.25 t nil))
		  0 :looped-p nil))

;;; Walking animation

(defparameter *human-male-walking-south*
  (make-animation '((0 64 32 128 0 nil nil)
		    (32 64 64 128 0.25 nil nil)
		    (0 64 32 128 0.5 nil nil)
		    (32 64 64 128 0.75 t nil))
		  1.0 :looped-p t))

(defparameter *human-male-walking-north*
  (make-animation '((64 64 96 128 0 nil nil)
		    (96 64 128 128 0.25 nil nil)
		    (64 64 96 128 0.5 nil nil)
		    (96 64 128 128 0.75 t nil))
		  1.0 :looped-p t))

(defparameter *human-male-walking-west*
  (make-animation '((128 64 160 128 0 nil nil)
		    (160 64 192 128 0.25 nil nil)
		    (192 64 224 128 0.5 nil nil)
		    (160 64 192 128 0.75 nil nil))
		  1.0 :looped-p t))

(defparameter *human-male-walking-east*
  (make-animation '((128 64 160 128 0 t nil)
		    (160 64 192 128 0.25 t nil)
		    (192 64 224 128 0.5 t nil)
		    (160 64 192 128 0.75 t nil))
		  1.0 :looped-p t))

(defparameter *human-male-walking-southwest*
  (make-animation '((224 64 256 128 0 nil nil)
		    (256 64 288 128 0.25 nil nil)
		    (288 64 320 128 0.5 nil nil)
		    (256 64 288 128 0.75 nil nil))
		  1.0 :looped-p t))

(defparameter *human-male-walking-southeast*
  (make-animation '((224 64 256 128 0 t nil)
		    (256 64 288 128 0.25 t nil)
		    (288 64 320 128 0.5 t nil)
		    (256 64 288 128 0.75 t nil))
		  1.0 :looped-p t))

(defparameter *human-male-walking-northwest*
  (make-animation '((320 64 352 128 0 nil nil)
		    (352 64 384 128 0.25 nil nil)
		    (384 64 416 128 0.5 nil nil)
		    (352 64 384 128 0.75 nil nil))
		  1.0 :looped-p t))

(defparameter *human-male-walking-northeast*
  (make-animation '((320 64 352 128 0 t nil)
		    (352 64 384 128 0.25 t nil)
		    (384 64 416 128 0.5 t nil)
		    (352 64 384 128 0.75 t nil))
		  1.0 :looped-p t))

(defun resolve-player-animation (player)
  (with-slots (direction moving-p) player
    (if moving-p
	(cond
	  ((eql direction 'south)
	   *human-male-walking-south*)
	  ((eql direction 'north)
	   *human-male-walking-north*)
	  ((eql direction 'west)
	   *human-male-walking-west*)
	  ((eql direction 'east)
	   *human-male-walking-east*)
	  ((eql direction 'southwest)
	   *human-male-walking-southwest*)
	  ((eql direction 'southeast)
	   *human-male-walking-southeast*)
	  ((eql direction 'northwest)
	   *human-male-walking-northwest*)
	  ((eql direction 'northeast)
	   *human-male-walking-northeast*))
	(cond
	  ((eql direction 'south)
	   *human-male-idle-south*)
	  ((eql direction 'north)
	   *human-male-idle-north*)
	  ((eql direction 'west)
	   *human-male-idle-west*)
	  ((eql direction 'east)
	   *human-male-idle-east*)
	  ((eql direction 'southwest)
	   *human-male-idle-southwest*)
	  ((eql direction 'southeast)
	   *human-male-idle-southeast*)
	  ((eql direction 'northwest)
	   *human-male-idle-northwest*)
	  ((eql direction 'northeast)
	   *human-male-idle-northeast*)))))
