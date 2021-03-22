#include "engine_capi.h"

enginePtr makeEngine(int w, int h) {
    return new SDLGraphicsProgram(w, h);
}

void clear(enginePtr ptr){
    ptr -> clear();

}

void flip(enginePtr ptr){
    ptr->flip();
}


void delay(enginePtr ptr, int milliseconds){
    ptr->delay(milliseconds);
}

void DrawRectangle(enginePtr ptr, int x, int y, int w, int h){
    ptr->DrawRectangle(x, y, w, h);
}
