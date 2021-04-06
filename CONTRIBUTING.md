# Engine Development

## Windows

* https://code.visualstudio.com/docs/cpp/config-mingw
* https://lazyfoo.net/tutorials/SDL/01_hello_SDL/windows/mingw/index.php
* install the sdl lib into a engine/lib folder
* install SDL2.dll into engine-lib/
* run make (or your make alternative) windows
* contribute!

## Linux

* https://lazyfoo.net/tutorials/SDL/01_hello_SDL/linux/index.php - follow this tutorial!
* run `make linux`
* contribute!

## Mac

### Brew
* ideally `make linux` can also work on Mac after setting up using `brew`
* https://www.libsdl.org/download-2.0.php go here to install both the runtime binaries and development libraries
* run `make linux`
* contribute!

### XCode
* follow https://lazyfoo.net/tutorials/SDL/01_hello_SDL/mac/index.php
* figure out how to create a .so file using xcode
* create a PR to update the Makefile to help support this for others!

## other notes

We want to transition into using CMake or something of the sort for easier development, but for the moment we're pleased that both linux and windows builds, for both the interpreter and engine, are working smoothly.

# Interpreter Development

## All platforms
* uses the stack build system
* run `stack build`
* run `./bin/PongChamp [file]` on Linux (or Mac, check the shebang)
* run `python \bin\PongChamp [file]` on Windows
* iterate
