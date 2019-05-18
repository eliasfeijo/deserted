;;;; tile.lisp
(in-package :deserted)

(define-image 'island "tileset/island.png")

(defparameter +flipped-horizontally-flag+ #x80000000)
(defparameter +flipped-vertically-flag+ #x40000000)
(defparameter +flipped-diagonally-flag+ #x20000000)

(defclass tile (positionable)
  ((flipped-horizontally-p :initarg :flipped-horizontally-p :reader flipped-horizontally-p)
   (flipped-vertically-p :initarg :flipped-vertically-p :reader flipped-vertically-p)
   (flipped-diagonally-p :initarg :flipped-diagonally-p :reader flipped-diagonally-p)
   (tileset :initarg :tileset :reader tileset-of)
   (tileset-index :initarg :tileset-index)
   (position-on-tileset :initarg :position-on-tileset :reader position-on-tileset-of)
   (tileset-image :accessor tileset-image-of)))

(defun position-of-tile-on-tileset (tileset-index tileset)
  (let* ((row-count (/ (tile-count-of tileset) (columns-of tileset)))
         (tileset-height (* (tile-height-of tileset) row-count))
         (current-row (floor (/ tileset-index row-count)))
         (position-x
          (*
           (tile-width-of tileset)
           (mod tileset-index (columns-of tileset))))
         (position-y
          (- tileset-height (* (+ current-row 1) (tile-height-of tileset)))))
    (vec2 position-x position-y)))

(defun make-tile (gid map)
  (let ((flipped-horizontally-p
         (not (equal (logand gid +flipped-horizontally-flag+) 0)))
        (flipped-vertically-p
         (not (equal (logand gid +flipped-vertically-flag+) 0)))
        (flipped-diagonally-p
         (not (equal (logand gid +flipped-diagonally-flag+) 0)))
        (gid-without-flags
         (logand gid (lognot (logior +flipped-horizontally-flag+ +flipped-vertically-flag+ +flipped-diagonally-flag+)))))
    (loop for tileset across (tilesets-of map) do
         (if (<= (first-gid-of tileset) gid-without-flags)
             (let ((tileset-index (- gid-without-flags (first-gid-of tileset))))
               (return (make-instance
                        'tile
                        :tileset-index tileset-index
                        :tileset tileset
                        :flipped-horizontally-p flipped-horizontally-p
                        :flipped-vertically-p flipped-vertically-p
                        :flipped-diagonally-p flipped-diagonally-p
                        :position-on-tileset (position-of-tile-on-tileset tileset-index tileset))))))))

(defmethod render ((this tile))
  (with-slots
        (tileset
         position-on-tileset
         flipped-horizontally-p
         flipped-vertically-p
         flipped-diagonally-p
         position
         tileset-image)
      this
    (let ((real-position position)
          (tile-width (tile-width-of tileset))
          (tile-height (tile-height-of tileset)))
      (with-pushed-canvas ()
        (cond
          (flipped-diagonally-p
           (rotate-canvas (- (/ pi 2)))
           (setf real-position (vec2 (+
                                      (- (y position))
                                      (- tile-height))
                                     (x position)))
           (if (and flipped-horizontally-p flipped-vertically-p)
               (progn
                 (scale-canvas -1 -1)
                 (setf real-position
                       (vec2
                        (+
                         (- (x real-position))
                         (- tile-width))
                        (+
                         (- (y real-position))
                         (- tile-height)))))
               (if flipped-horizontally-p
                   (progn
                     (scale-canvas -1 1)
                     (setf real-position
                           (vec2
                            (+
                             (- (x real-position))
                             (- tile-width))
                            (y real-position))))
                   (if flipped-vertically-p
                       (progn
                         (scale-canvas -1 -1)
                         (setf real-position
                               (vec2
                                (+
                                 (- (x real-position))
                                 (- tile-width))
                                (+
                                 (- (y real-position))
                                 (- tile-height)))))))))
          (flipped-horizontally-p
           (if flipped-vertically-p
               (progn
                 (scale-canvas -1 -1)
                 (setf real-position
                       (vec2
                        (+
                         (- (x real-position))
                         (- tile-width))
                        (+
                         (- (y real-position))
                         (- tile-height)))))
               (progn
                 (scale-canvas -1 1)
                 (setf real-position
                       (vec2
                        (+
                         (- (x real-position))
                         (- tile-width))
                        (y real-position))))))
          (flipped-vertically-p
           (progn
             (scale-canvas 1 -1)
             (setf real-position
                   (vec2
                    (x real-position)
                    (+
                     (- (y real-position))
                     (- tile-height)))))))
        (draw-image
         real-position
         tileset-image
         :origin position-on-tileset
         :width tile-width
         :height tile-height)))))
