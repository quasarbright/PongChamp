# PongChamp

Welcome to our PongChamp language! This was a project that was started as a weekend thing and we plan to keep expanding on it to integrate a simple but feature-full game engine. The goal of the language is to be coupled tightly to the engine so that it can make it as easy as possible to script with it.

## Building
* `make all` will build the engine library for mac and windows, assuming you have the correct development libraries installed. A prebuilt version is already uploaded though.
* `stack build` is necessary for now to build your own executable for your target architecture

## Build Nuances
* unfortunately currently it is required to have the sdl libs installed to your system, including `sdl2_ttf`, `sdl2_mixer`, `sdl2`, `sdl2_image`
* The engine will eventually move into an open-gl based engine, while the base design of the engine is decided on a temporary engine is in place to demo functionality of the language.
* Windows builds will require more effort to ensure proper c libs are installed
* In order to build the windows target `mingw-w64` is needed

## Running
The simplest way to do this is to run `./bin/PongChamp [file]`. This ensures your library path is appropriately set. Since the engine-lib is not installed to your machines known library path, `LD_LIBRARY_PATH` needs to be set for `stack` to function correctly. If you want to run with `stack`, this may serve your purposes: `LD_LIBRARY_PATH=$(pwd)/engine-lib/ stack run -- [file]`. Note that for a Windows build to function properly the `PATH` variable needs to be set, rather than `LD_LIBRARY_PATH`. The python script also does this currently.

## Current features
* Loops
* Datatypes: Number, String, Boolean
* Higher order functions
* Branching
* A set of unary and binary operators
* Variable assignment
* No module system

## Integration with C++ engine
Haskell provides a FFI interface to interface with other languages. However, in order to interface with a C++ library, it is necessary to write a c wrapper around the engine and then interface with C. Currently, we have a thin demo engine which serves as an example of this. In the examples directory are multiple scripts testing (or demonstrating errors) the language, to view an example "game" run [full-engine.js](examples/full-engine.js). This follows the general desired format of the future language features, however some of the minor details which actually make the game, like passing around the engine pointer, or the main game loop, will be abstracted in a style similar to P5.js. As an example of a more detailed engine that will eventually be integrated, refer to [this example](https://ryanrio.github.io/game-engines-docs/Assignment2_Docs/index.html).

## Known Issues
* The biggest issue is haskell errors are being swallowed
* No garbage collection

## Other Notes
* Many developer notes are contained in [this directory](notes/).
* The first major version of PongChamp will be after sufficient language features are implemented.