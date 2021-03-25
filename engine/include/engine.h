#ifndef __ENGINECPP_H__
#define __ENGINECPP_H__

#ifdef __cplusplus

#include <SDL2/SDL.h>

// The glad library helps setup OpenGL extensions.
#include <glad/glad.h>

#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <vector>
enum KeyEvent {
    QUIT,
    KeyUP,
    KeyDOWN
};

class SDLGraphicsProgram{
public:

    // Constructor
    SDLGraphicsProgram(int w, int h);
    // Destructor
    ~SDLGraphicsProgram();
    // Setup OpenGL
    bool initGL();
    // Clears the screen
    void clear();
    // Flips to new buffer
    void flip();
    // Delay rendering
    void delay(int milliseconds);
    // Get Pointer to Window
    SDL_Window* getSDLWindow();
    // Draw a simple rectangle
    void DrawRectangle(int x, int y, int w, int h);
    // poll and get inputs
    std::vector<std::pair<char, Uint32>> getInputs();

private:
    // Screen dimension constants
    int screenHeight;
    int screenWidth;
    // The window we'll be rendering to
    SDL_Window* gWindow ;
    // Our renderer
    SDL_Renderer* gRenderer;
};

#endif // !__cplusplus

#endif // !__ENGINECPP_H__