# Common Lisp Game

A simple game, using [trivial-gamekit](https://github.com/borodust/trivial-gamekit) and [Tiled Map Editor](https://www.mapeditor.org/) made for [Mini Jam 28 - Pirates](https://itch.io/jam/mini-jam-28-pirates).

Binaries available at [releases](https://github.com/eliasfeijo/deserted/releases) page and at my [itch.io](https://efeijo.itch.io/deserted) page.

## Requirements

* OpenGL 3.3+
* 64-bit (x86_64) Windows, GNU/Linux or macOS

## Compile it yourself

Requires

* x86_64 SBCL or CCL
* Quicklisp

Git clone into Quicklisp "local-projects" directory:

`git clone https://github.com/eliasfeijo/deserted.git $HOME/quicklisp/local-projects/deserted`

Run
```
(ql:quickload :deserted)
(in-package :deserted)
(play)
```

### Controls

WASD - Movement

Spacebar - Action