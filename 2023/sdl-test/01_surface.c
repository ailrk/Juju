#include <stdio.h>
#include <stdbool.h>


#include <SDL2/SDL.h>


const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 640;


typedef enum {
    KEY_PRESS_SURFACE_DEFAULT, 
    KEY_PRESS_SURFACE_UP, 
    KEY_PRESS_SURFACE_DOWN, 
    KEY_PRESS_SURFACE_LEFT, 
    KEY_PRESS_SURFACE_RIGHT, 
    KEY_PRESS_SURFACE_TOTAL, 
} KeyPressSurfaces;


typedef struct {
    SDL_Window *window;
    SDL_Surface *screenSurface;
    SDL_Surface *keyPressSurfaces[KEY_PRESS_SURFACE_TOTAL];
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



SDL_Surface * loadSurface (Context *context, const char *path) {
    SDL_Surface *optimizedSurface = NULL;

    SDL_Surface* loadedSurface = SDL_LoadBMP(path);
    if (!loadedSurface) {
        printf("Unable to load images %s, SDL Error %s\n", path, SDL_GetError());
        return NULL;
    }

    optimizedSurface = SDL_ConvertSurface(loadedSurface, context->screenSurface->format, 0);
    if (!optimizedSurface) {
        printf("Unable to optmize images %s, SDL Error %s\n", path, SDL_GetError());
        return NULL;
    }

    SDL_FreeSurface(loadedSurface);


    return optimizedSurface;
}


/* load image */
bool loadMedia (Context *context) {
    context->keyPressSurfaces [KEY_PRESS_SURFACE_DEFAULT] = loadSurface(context, "kirby.bmp");
    context->keyPressSurfaces [KEY_PRESS_SURFACE_UP] = loadSurface(context, "up.bmp");
    context->keyPressSurfaces [KEY_PRESS_SURFACE_DOWN] = loadSurface(context, "down.bmp");
    context->keyPressSurfaces [KEY_PRESS_SURFACE_LEFT] = loadSurface(context, "left.bmp");
    context->keyPressSurfaces [KEY_PRESS_SURFACE_RIGHT] = loadSurface(context, "right.bmp");

    for (int i = KEY_PRESS_SURFACE_UP; i < KEY_PRESS_SURFACE_TOTAL; ++i) {
        if (!context->keyPressSurfaces[i]) {
            printf("failed to load keypress surface, SDL_Error %s\n", SDL_GetError());
            return false;
        }
    }

    context->currentSurface = context->keyPressSurfaces [KEY_PRESS_SURFACE_DEFAULT];
    return true;
}


void freeContext(Context *context) {
    SDL_FreeSurface(context->screenSurface); context->screenSurface = NULL;
    for (int i = KEY_PRESS_SURFACE_UP; i < KEY_PRESS_SURFACE_TOTAL; ++i) {
        SDL_FreeSurface(context->keyPressSurfaces[i]);
        context->keyPressSurfaces[i] = NULL;
    }
    SDL_DestroyWindow(context->window); context->window = NULL;
}


void quit(Context *context) {
    freeContext(context);
    SDL_Quit();
}

int main(int argc, char *argv[]) {
    Context context;

    if (!init(&context)) quit(&context);
    if (!loadMedia(&context)) quit(&context);
    SDL_Event e;


    bool quit = false;
    while(!quit) {
        while ( SDL_PollEvent (&e) != 0) {
            if (e.type == SDL_QUIT) {
                quit = true;
                break;
            }

            if (e.type == SDL_KEYDOWN) {
                switch(e.key.keysym.sym) {
                    case SDLK_UP:
                        context.currentSurface = context.keyPressSurfaces[KEY_PRESS_SURFACE_UP];
                        break;

                    case SDLK_DOWN:
                        context.currentSurface = context.keyPressSurfaces[KEY_PRESS_SURFACE_DOWN];
                        break;

                    case SDLK_LEFT:
                        context.currentSurface = context.keyPressSurfaces[KEY_PRESS_SURFACE_LEFT];
                        break;

                    case SDLK_RIGHT:
                        context.currentSurface = context.keyPressSurfaces[KEY_PRESS_SURFACE_RIGHT];
                        break;

                    default:
                        context.currentSurface = context.keyPressSurfaces[KEY_PRESS_SURFACE_DEFAULT];
                        break;
                }
            }
        }

        SDL_Rect stretchRect;
        stretchRect.x = 0;
        stretchRect.y = 0;
        stretchRect.w = SCREEN_WIDTH;
        stretchRect.h = SCREEN_HEIGHT;

        SDL_BlitScaled(context.currentSurface, NULL, context.screenSurface, &stretchRect);
        SDL_UpdateWindowSurface(context.window);
    }
    return 0;
}

