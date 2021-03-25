#include "engine.h"

SDLGraphicsProgram::SDLGraphicsProgram(int w, int h):screenWidth(w),screenHeight(h){
    printf("initializing sdl\n");
	bool success = true;
	std::stringstream errorStream;
	gWindow = NULL;
	// Render flag

	// Initialize SDL
	if(SDL_Init(SDL_INIT_VIDEO)< 0){
		errorStream << "SDL could not initialize! SDL Error: " << SDL_GetError() << "\n";
		success = false;
	}
	else{
	    //Create window
    	gWindow = SDL_CreateWindow( "Lab", 100, 100, screenWidth, screenHeight, SDL_WINDOW_SHOWN );

        // Check if Window did not create.
        if( gWindow == NULL ){
			errorStream << "Window could not be created! SDL Error: " << SDL_GetError() << "\n";
			success = false;
		}

		//Create a Renderer to draw on
        gRenderer = SDL_CreateRenderer(gWindow, -1, SDL_RENDERER_ACCELERATED);
        // Check if Renderer did not create.
        if( gRenderer == NULL ){
            errorStream << "Renderer could not be created! SDL Error: " << SDL_GetError() << "\n";
            success = false;
        }
  	}

    if(!success){
        errorStream << "SDLGraphicsProgram::SDLGraphicsProgram - Failed to initialize!\n";
        std::string errors=errorStream.str();
        SDL_Log("%s\n",errors.c_str());
    }else{
        SDL_Log("SDLGraphicsProgram::SDLGraphicsProgram - No SDL, GLAD, or OpenGL, errors detected during initialization\n\n");
    }

}

SDLGraphicsProgram::~SDLGraphicsProgram(){
	SDL_DestroyWindow( gWindow );
	gWindow = NULL;
	SDL_Quit();
}

bool SDLGraphicsProgram::initGL(){
	//Success flag
	bool success = true;

	return success;
}

void SDLGraphicsProgram::clear(){
    SDL_SetRenderDrawColor(gRenderer, 0x44,0x44,0x4,0xFF);
    SDL_RenderClear(gRenderer);   
}

void SDLGraphicsProgram::flip(){
    SDL_RenderPresent(gRenderer);
}

void SDLGraphicsProgram::delay(int milliseconds){
    SDL_Delay(milliseconds); 
}

SDL_Window* SDLGraphicsProgram::getSDLWindow(){
  return gWindow;
}

void SDLGraphicsProgram::DrawRectangle(int x, int y, int w, int h){
    SDL_Rect fillRect = {x,y,w,h};
    SDL_SetRenderDrawColor(gRenderer, 0xFF, 0x00, 0x00, 0xFF);
    SDL_RenderDrawRect(gRenderer, &fillRect); 
}

std::vector<std::pair<char, Uint32>> SDLGraphicsProgram::getInputs()
{
    SDL_StartTextInput();
    SDL_Event e;
    bool quit;
    std::vector<std::pair<char, Uint32>> inputs;
    //Handle events on queue
    while (SDL_PollEvent(&e) != 0)
    {
        // User posts an event to quit
        // An example is hitting the "x" in the corner of the window.
        if (e.type == SDL_QUIT)
        {
            quit = true;
        }
        inputs.push_back(std::pair<char, Uint32>(e.key.keysym.sym, e.type));
    } // End SDL_PollEvent loop.
    return inputs;
    //Disable text input
    SDL_StopTextInput();
};




