;;;; world.lisp
(cl:in-package :deserted)

(defclass key (positionable)
  ((size :initform (vec2 50 50) :reader size-of)
   (in-inventory-p :initform nil :accessor in-inventory-p)))

(defclass chest (positionable)
  ((size :initform (vec2 32 32) :reader size-of)
   (open-p :initform nil :accessor open-p)))

(defclass world ()
  ((map :initarg :map :reader map-of)
   (grid :initarg :grid)
   (skeleton-spawn-positions :initarg :skeleton-spawn-positions)
   (enemies :reader enemies-of)
   (player-initial-position :initarg :player-initial-position)
   (key-positions :initarg :key-positions)
   (key :reader key-of)
   (chest :reader chest-of)
   (player :reader player-of)))

(defmethod initialize-instance :after ((this world) &key)
  (clear-world this))

(defun clear-world (world)
  (with-slots (map grid player player-initial-position
                   enemies skeleton-spawn-positions
                   key-positions key chest)
      world
    (setf player (make-instance 'player :position player-initial-position)
          enemies (make-array 0 :fill-pointer 0 :adjustable t)
          key (make-instance 'key :position (aref key-positions (random (length key-positions))))
          chest (make-instance 'chest :position (vec2
                                                 (+ (x (position-of player)) (x (size-of player)))
                                                 (y (position-of player)))))
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
                          (name-of (image-of (tileset-of tile)))) :deserted)))
                  (setf
                   (position-of tile) (vec2 position-x position-y)
                   (tileset-image-of tile) tileset-image)))))
    (loop for spawn-position across skeleton-spawn-positions do
         (vector-push-extend (make-instance 'skeleton-spear :position spawn-position) enemies))))

(defmethod render ((this world))
  (with-slots (map grid player enemies key chest) this
    ;; Will do the rendering from the bottom-left corner of screen
    (loop for y downfrom (- (height-of map) 1) to 0 do
         (loop for x from 0 below (width-of map) do
              (let ((tile (aref grid y x)))
                (render tile))))
    (if (not (in-inventory-p key))
        (draw-image (position-of key) 'key))
    (if (open-p chest)
        (draw-image (position-of chest) 'chest
                    :origin (vec2 32 0)
                    :width 32 :height 32)
        (draw-image (position-of chest) 'chest
                    :origin (vec2 0 0)
                    :width 32 :height 32))
    (loop for enemy across enemies do
         (when (>= (y (position-of enemy)) (y (position-of player)))
           (render enemy)))
    (render player)
    (loop for enemy across enemies do
         (when (< (y (position-of enemy)) (y (position-of player)))
           (render enemy)))))
