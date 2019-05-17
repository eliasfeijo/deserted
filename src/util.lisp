;;;; util.lisp
(cl:in-package :deserted)

(defparameter *viewport-width* 800)
(defparameter *viewport-height* 600)
(defparameter *viewport-origin* (vec2 0 0))

(defparameter *white* (vec4 1 1 1 1))
(defparameter *black* (vec4 0 0 0 1))

(defparameter *assets-path* (asdf:system-relative-pathname :deserted "assets/"))
