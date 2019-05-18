;;;; fog.lisp
(cl:in-package :deserted)

(define-image 'fog "images/fog.png")

(defclass fog (renderable positionable)
  ((position1 :initform (vec2 0 0))
   (position2 :initform (vec2 0 0))
   (size :initform (vec2 0 0))
   (velocity :initform (vec2 -100 0))))

(defmethod initialize-instance :after ((this fog) &key)
  (with-slots (size position2) this
    (setf size (vec2 (image-width 'fog)
                     (image-height 'fog))
          position2 (vec2 (- (x size) 180) 0))))

(defun update-fog (fog delta-time)
  (with-slots (position1 position2 size velocity) fog
    (let ((real-speed (mult velocity delta-time)))
      (cond ((< (x position1) (- (x size)))
             (setf (x position1) (+ (x position2) (- (x size) 180))))
            ((< (x position2) (- (x size)))
             (setf (x position2) (+ (x position1) (- (x size) 180)))))
      (setf position1 (add position1 real-speed)
            position2 (add position2 real-speed)))))

(defmethod render ((this fog))
  (with-slots (position1 position2 size) this
    (with-pushed-canvas ()
      (scale-canvas 4 3.5)
      (translate-canvas 0 -100)
      (draw-image position1 'fog)
      (draw-image position2 'fog))))
