#ifndef __ENGINECPP_CAPI_H__
#define __ENGINECPP_CAPI_H__

#include "engine.h"

#ifdef __cplusplus
extern "C" {
typedef SDLGraphicsProgram *enginePtr;
#else
typedef void *APtr; // dummy type.
#endif

enginePtr makeEngine(int w, int h);

void clear(enginePtr ptr);

void flip(enginePtr ptr);

void delay(enginePtr ptr, int milliseconds);

void DrawRectangle(enginePtr ptr, int x, int y, int w, int h);

#ifdef __cplusplus
}
#endif

#endif  // !__ACPP_CAPI_H__