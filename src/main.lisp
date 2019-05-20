;;;; main.lisp
(cl:in-package :deserted)

(register-resource-package :deserted *assets-path*)

(define-image 'key "images/key.png")
(define-image 'chest "images/chest.png")

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
    (bind-any-button
     (lambda (key state)
       (cond ((eql state :pressed)
              (press-key game-state key))
             ((eql state :released)
              (release-key game-state key)))))
    (prepare-resources
     'island
     'pirate
     'fog
     'skeleton-spear
     'key
     'chest)))

(defmethod notice-resources ((this deserted) &rest resource-names)
  (declare (ignore resource-names))
  (with-slots (game-state map grid world) this
    (let* ((layer1 (aref (layers-of map) 0))
           (data (data-of layer1))
           (player-initial-position (vec2 0 0))
           (skeleton-spawn-positions (make-array 0 :fill-pointer 0 :adjustable t))
           (key-positions (make-array 4 :fill-pointer 0))
           (real-map-height
            (*
             (height-of map)
             (tile-height-of map))))
      (loop for object-group across (object-groups-of map) do
           (loop for object across (objects-of object-group) do
                (with-slots (name x y height) object
                  (cond
                    ((string-equal name "key")
                     (vector-push (vec2 x (- real-map-height height)) key-positions))
                    ((string-equal name "player_spawn_position")
                     (setf player-initial-position
                           (vec2 x (- real-map-height y height))))
                    ((string-equal name "skeleton_spawn_position")
                     (vector-push-extend (vec2 x (- real-map-height y height)) skeleton-spawn-positions))))))
      (setf grid (make-array (list (height-of map) (width-of map)) :initial-contents data))
      (loop for y from 0 below (height-of map) do
           (loop for x from 0 below (width-of map) do
                (let ((tile (make-tile (elt (elt data y) x) map)))
                  (setf (aref grid y x) tile)))
         finally
           (setf world (make-instance 'world
                                      :map map
                                      :grid grid
                                      :player-initial-position player-initial-position
                                      :key-positions key-positions
                                      :skeleton-spawn-positions skeleton-spawn-positions)
                 game-state (make-instance 'game
                                           :world world))))))

(defmethod act ((this deserted))
  (with-slots (game-state) this
    (act game-state)))

(defmethod draw ((this deserted))
  (with-slots (game-state) this
    (render game-state)))

(defun play (&optional blocking)
  (gamekit:start 'deserted :blocking blocking))
