(asdf:defsystem "deserted"
    :description "Game made for Mini Jam 2019-05-17"
    :version "0.0.1"
    :author "Elias Feijó"
    :license "MIT"
    :depends-on (alexandria bodge-utilities trivial-gamekit)
    :serial t
    :pathname "src/"
    :components ((:file "packages")
                 (:file "main")))
