;;;; math.lisp
(cl:in-package :deserted)

(defun intersect-p (rect1 rect2)
  (if (and
       (> (+ (x rect1) (z rect1)) (x rect2))
       (< (x rect1) (+ (x rect2) (z rect2))))
      (if (and
           (> (+ (y rect1) (w rect1)) (y rect2))
           (< (y rect1) (+ (y rect2) (w rect2))))
          t)))

(defun distance (origin target)
  (let ((difference (subt origin target)))
    (sqrt (+
           (* (x difference) (x difference))
           (* (y difference) (y difference))))))
