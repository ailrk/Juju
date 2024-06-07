#include <stdio.h>
#include <stdbool.h>
#include <SDL2/SDL.h>

const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 640;


typedef struct {
    SDL_Window *window;
    SDL_Surface *screenSurface;
    SDL_Surface *currentSurface;
} Context;



/* initialize the SDL context */
bool init(Context *context) {

    if (SDL_Init (SDL_INIT_VIDEO) < 0) {
        printf("SDL could not initialize! SDL Error %s\n", SDL_GetError());
        return false;
    }

    context->window = SDL_CreateWindow("two pieces", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN);

    if (!context->window) {
        printf("Window can't be created, SDL Error%s\n", SDL_GetError());
        return false;
    }

    context->screenSurface = SDL_GetWindowSurface(context->window);
    return true;
}
