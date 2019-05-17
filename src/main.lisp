;;;; main.lisp
(cl:in-package :deserted)

(register-resource-package :deserted *assets-path*)

(defgame deserted ()
  ((game-state)
   (map :initform nil)
   (grid :initform nil)
   (world :initform nil))
  (:viewport-width *viewport-width*)
  (:viewport-height *viewport-height*)
  (:viewport-title "Deserted")
  (:prepare-resources nil))

(defmethod post-initialize :after ((this deserted))
  (with-slots (game-state map) this
    (setf game-state (make-instance 'resource-preparation)
          map (parse-tmx (merge-pathnames "map/map1.tmx" *assets-path*)))
    (prepare-resources
     'island)))

(defmethod notice-resources ((this deserted) &rest resource-names)
  (declare (ignore resource-names))
  (with-slots (game-state map grid world) this
    (let* ((layer1 (aref (layers-of map) 0))
           (data (data-of layer1))
           (player-initial-position (vec2 0 0))
           (real-map-height
            (*
             (height-of map)
             (tile-height-of map))))
      (loop for object-group across (object-groups-of map) do
           (loop for object across (objects-of object-group) do
                (with-slots (name x y height) object
                  (cond
                    ((string-equal name "player_spawn_position")
                     (setf player-initial-position
                           (vec2 x (- real-map-height y height))))))))
      (setf grid (make-array (list (height-of map) (width-of map)) :initial-contents data))
      (loop for y from 0 below (height-of map) do
           (loop for x from 0 below (width-of map) do
                (let ((tile (make-tile (elt (elt data y) x) map)))
                  (setf (aref grid y x) tile)))
         finally
           (setf world (make-instance 'world :map map :grid grid)
                 game-state (make-instance 'game
                                           :world world
                                           :player-initial-position player-initial-position))))))

(defmethod draw ((this deserted))
  (with-slots (game-state) this
    (render game-state)))

(defun play (&optional blocking)
  (gamekit:start 'deserted :blocking blocking))
