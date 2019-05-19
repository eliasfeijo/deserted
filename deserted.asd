(asdf:defsystem "deserted"
    :description "Game made for Mini Jam 2019-05-17"
    :version "0.0.1"
    :author "Elias Feijó"
    :license "MIT"
    :depends-on (alexandria
                 bodge-utilities
                 trivial-gamekit
                 cxml
                 cl-csv)
    :serial t
    :pathname "src/"
    :components ((:file "packages")
                 (:file "util")
                 (:file "math")
                 (:file "tmx-parser")
                 (:file "tile")
                 (:file "fog")
                 (:file "animation")
                 (:file "animation-skeleton")
                 (:file "skeleton-spear")
                 (:file "animation-player")
                 (:file "player")
                 (:file "camera")
                 (:file "world")
                 (:file "state")
                 (:file "main")))
