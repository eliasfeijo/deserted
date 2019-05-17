;;;; tmx-parser.lisp
(in-package :deserted)

(defclass game-map ()
  ((width :initarg :width :reader width-of)
   (height :initarg :height :reader height-of)
   (tile-width :initarg :tile-width :reader tile-width-of)
   (tile-height :initarg :tile-height :reader tile-height-of)
   (tilesets :initarg :tilesets :reader tilesets-of)
   (layers :initarg :layers :reader layers-of)
   (object-groups :initarg :object-groups :reader object-groups-of)))

(defclass tileset ()
  ((first-gid :initarg :first-gid :reader first-gid-of)
   (name :initarg :name :reader name-of)
   (tile-width :initarg :tile-width :reader tile-width-of)
   (tile-height :initarg :tile-height :reader tile-height-of)
   (tile-count :initarg :tile-count :reader tile-count-of)
   (columns :initarg :columns :reader columns-of)
   (image :initarg :image :reader image-of)))

(defclass tileset-image ()
  ((name :initarg :name :reader name-of)
   (path :initarg :path :reader path-of)
   (width :initarg :width :reader width-of)
   (height :initarg :height :reader width-of)))

(defclass layer ()
  ((id :initarg :id)
   (name :initarg :name)
   (width :initarg :width)
   (height :initarg :height)
   (data :initarg :data :reader data-of)))

(defclass map-object ()
  ((id :initarg :id)
   (name :initarg :name)
   (gid :initarg :gid)
   (x :initarg :x)
   (y :initarg :y)
   (width :initarg :width)
   (height :initarg :height)))

(defclass object-group ()
  ((id :initarg :id)
  (name :initarg :name :reader name-of)
  (objects :initarg :objects :reader objects-of)))

(defun parse-tmx (file-path)
  (let* ((document (cxml:parse-file file-path (cxml-dom:make-dom-builder)))
	 (map-dom (dom:document-element document)))
    (extract-map-from-dom map-dom)))

(defun extract-map-from-dom (map-dom)
  (let ((width (parse-integer (dom:get-attribute map-dom "width")))
        (height (parse-integer (dom:get-attribute map-dom "height")))
        (tile-width (parse-integer (dom:get-attribute map-dom "tilewidth")))
        (tile-height (parse-integer (dom:get-attribute map-dom "tileheight")))
        (child-nodes (dom:child-nodes map-dom))
        (tilesets (make-array 0 :fill-pointer 0 :adjustable t))
        (layers (make-array 0 :fill-pointer 0 :adjustable t))
        (object-groups (make-array 0 :fill-pointer 0 :adjustable t)))
    (loop
       for item across child-nodes
       do (if (dom:element-p item)
              (cond
                ((string-equal (dom:tag-name item) "tileset")
                 (if (dom:has-attribute item "source")
                     (vector-push-extend (extract-tileset-from-tsx (parse-integer (dom:get-attribute item "firstgid")) (merge-pathnames (concatenate 'string "map/" (dom:get-attribute item "source")) *assets-path*)) tilesets)
                     (vector-push-extend (extract-tileset-from-tag (parse-integer (dom:get-attribute item "firstgid")) item) tilesets)))
                ((string-equal (dom:tag-name item) "layer")
                 (vector-push-extend (extract-layer-from-tag item) layers))
                ((string-equal (dom:tag-name item) "objectgroup")
                 (vector-push-extend (extract-object-group-from-tag item) object-groups)))))
    (make-instance 'game-map :width width :height height :tile-width tile-width :tile-height tile-height :tilesets (make-array (length tilesets) :initial-contents tilesets) :layers (make-array (length layers) :initial-contents layers) :object-groups object-groups)))

(defun extract-object-group-from-tag (item)
  (let ((id (dom:get-attribute item "id"))
        (name (dom:get-attribute item "name"))
        (child-nodes (dom:child-nodes item))
        (objects (make-array 0 :fill-pointer 0 :adjustable t)))
    (loop
       for object across child-nodes
       do (if (dom:element-p object)
              (vector-push-extend (extract-object-from-tag object) objects)))
    (make-instance 'object-group :id id :name name :objects objects)))

(defun extract-object-from-tag (item)
  (let ((id (dom:get-attribute item "id"))
        (name (dom:get-attribute item "name"))
        (gid (parse-integer (dom:get-attribute item "gid")))
        (x (parse-integer (dom:get-attribute item "x")))
        (y (parse-integer (dom:get-attribute item "y")))
        (width (parse-integer (dom:get-attribute item "width")))
        (height (parse-integer (dom:get-attribute item "height"))))
    (make-instance 'map-object
                   :id id
                   :name name
                   :gid gid
                   :x x
                   :y y
                   :width width
                   :height height)))

(defun extract-layer-from-tag (item)
  (let* ((id (dom:get-attribute item "id"))
	 (name (dom:get-attribute item "name"))
	 (width (parse-integer (dom:get-attribute item "width")))
	 (height (parse-integer (dom:get-attribute item "height")))
	 (data-element (aref (dom:get-elements-by-tag-name item "data") 0))
	 (raw-data (dom:node-value (dom:first-child data-element)))
	 (data (remove-if #'null
			  (cl-csv:read-csv raw-data
					   :unquoted-empty-string-is-nil t
					   :map-fn #'(lambda (row)
						       (remove-if #'null row))
					   :data-map-fn #'(lambda (datum &key &allow-other-keys)
							    (unless (string= datum "")
							      (parse-integer datum)))))))
    (make-instance 'layer :id id :name name :width width :height height :data data)))

(defun extract-tileset-from-tsx (first-gid file-path)
  (let* ((document (cxml:parse-file file-path (cxml-dom:make-dom-builder)))
	 (tileset-element (dom:document-element document)))
    (extract-tileset-from-tag first-gid tileset-element)))

(defun extract-tileset-from-tag (first-gid item)
  (let* ((name (dom:get-attribute item "name"))
	 (tile-width (parse-integer (dom:get-attribute item "tilewidth")))
	 (tile-height (parse-integer (dom:get-attribute item "tileheight")))
	 (tile-count (parse-integer (dom:get-attribute item "tilecount")))
	 (columns (parse-integer (dom:get-attribute item "columns")))
	 (image-element (aref (dom:get-elements-by-tag-name item "image") 0))
	 (image-source (merge-pathnames (concatenate 'string "tileset/" (dom:get-attribute image-element "source")) *assets-path*))
	 (image
	  (make-instance 'tileset-image
			 :name (pathname-name image-source)
			 :path image-source
			 :width (parse-integer (dom:get-attribute image-element "width"))
			 :height (parse-integer (dom:get-attribute image-element "height")))))
    (make-instance 'tileset :first-gid first-gid :name name :image image :columns columns :tile-count tile-count :tile-width tile-width :tile-height tile-height)))
