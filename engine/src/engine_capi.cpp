#include "engine_capi.h"

enginePtr makeEngine(int w, int h) {
    return new SDLGraphicsProgram(w, h);
}