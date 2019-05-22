# Common Lisp Game

A simple game, using [trivial-gamekit](https://github.com/borodust/trivial-gamekit) and [Tiled Map Editor](https://www.mapeditor.org/) made for [Mini Jam 28 - Pirates](https://itch.io/jam/mini-jam-28-pirates).

Binaries available at [releases](https://github.com/eliasfeijo/deserted/releases) page and at my [itch.io](https://efeijo.itch.io/deserted) page.

## Requirements

* OpenGL 2.1+
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

* WASD - Movement

* Spacebar - Action

## Credits

### Programming and Idea

Elias Feijó

### Image Credits

The tileset (3 tiles) I made myself with [Aseprite](https://www.aseprite.org/).

Player and Skeleton sprites are from the Universal LPC Character Generator - http://gaurav.munjal.us/Universal-LPC-Spritesheet-Character-Generator/

The Key sprite is from BizmasterStudios - https://opengameart.org/content/key-icons

The Chest sprite is from Blarumyrran - https://opengameart.org/content/modified-32x32-treasure-chest

The Fog texture is from Santoniche - https://opengameart.org/content/mist

### Sound Credits

Intro sound - [scary](https://freesound.org/people/Kastenfrosch/sounds/162472/) by Kastenfrosch

Music - [Times of Unrest](https://freesound.org/people/Setuniman/sounds/470116/) by Setuniman

Skeleton rise - [Bones and Flesh Movement](https://freesound.org/people/Aurelon/sounds/422622/) by Aurelon

Skeleton attack - [Attack - Kick](https://freesound.org/people/elynch0901/sounds/464501/) by elynch0901

Player attack - [Attack -Punch](https://freesound.org/people/elynch0901/sounds/464503/) by elynch0901

Skeleton die - [Die](https://freesound.org/people/89o/sounds/434129/) by 89o

Player die - [Man Dying](https://freesound.org/people/Under7dude/sounds/163442/)  by Under7dude

Win - [Jingle_Win_00](https://freesound.org/people/LittleRobotSoundFactory/sounds/270402/) by LittleRobotSoundFactory
