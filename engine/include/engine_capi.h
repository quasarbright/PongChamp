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
// void applyStateA(enginePtr engine, i2i arg);
// int getStateA(enginePtr engine);
// void setStateA(enginePtr engine, int arg);
// void freeA(enginePtr engine);

#ifdef __cplusplus
}
#endif

#endif  // !__ACPP_CAPI_H__