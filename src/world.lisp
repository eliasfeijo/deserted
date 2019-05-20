;;;; world.lisp
(cl:in-package :deserted)

(defclass world ()
  ((map :initarg :map :reader map-of)
   (grid :initarg :grid)
   (skeleton-spawn-positions :initarg :skeleton-spawn-positions)
   (enemies :reader enemies-of)
   (player-initial-position :initarg :player-initial-position)
   (player :reader player-of)))

(defmethod initialize-instance :after ((this world) &key)
  (clear-world this))

(defun clear-world (world)
  (with-slots (map grid player player-initial-position
                   enemies skeleton-spawn-positions)
      world
    (setf player (make-instance 'player :position player-initial-position)
          enemies (make-array 0 :fill-pointer 0 :adjustable t))
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
                   (tileset-image-of tile) tileset-image)))))
    (loop for spawn-position across skeleton-spawn-positions do
         (vector-push-extend (make-instance 'skeleton-spear :position spawn-position) enemies))))

(defmethod render ((this world))
  (with-slots (map grid player enemies) this
    ;; Will do the rendering from the bottom-left corner of screen
    (loop for y downfrom (- (height-of map) 1) to 0 do
         (loop for x from 0 below (width-of map) do
              (let ((tile (aref grid y x)))
                (render tile))))
    (loop for enemy across enemies do
         (when (>= (y (position-of enemy)) (y (position-of player)))
           (render enemy)))
    (render player)
    (loop for enemy across enemies do
         (when (< (y (position-of enemy)) (y (position-of player)))
           (render enemy)))))
