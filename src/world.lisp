;;;; world.lisp
(cl:in-package :deserted)

(defclass world ()
  ((map :initarg :map :reader map-of)
   (grid :initarg :grid)))

(defmethod initialize-instance :after ((this world) &key)
  (with-slots (map grid) this
    (let ((real-map-height
           (*
            (height-of map)
            (tile-height-of map))))
      (loop for y downfrom (- (height-of map) 1) to 0 do
           (loop for x from 0 below (width-of map) do
                (let* ((tile (aref grid y x))
                       (position-y
                        (-
                         real-map-height
                         (* (tile-height-of map) (+ y 1))))
                       (position-x
                        (* (tile-width-of map) x))
                       (tileset-image
                        (find-symbol
                         (string-upcase
                          (name-of (image-of (tileset-of tile)))))))
                  (setf
                   (position-of tile) (vec2 position-x position-y)
                   (tileset-image-of tile) tileset-image)))))))

(defmethod render ((this world))
  (with-slots (map grid) this
    ;; Will do the rendering from the bottom-left corner of screen
    (loop for y downfrom (- (height-of map) 1) to 0 do
	 (loop for x from 0 below (width-of map) do
	      (let ((tile (aref grid y x)))
		(render tile))))))