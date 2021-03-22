#ifndef __ACPP_CAPI_H__
#define __ACPP_CAPI_H__

#include "acpp.h"

#ifdef __cplusplus
extern "C" {
typedef A *APtr;
#else
typedef void *APtr; // dummy type.
#endif

APtr makeA(int v);
void applyStateA(APtr aptr, i2i arg);
int getStateA(APtr aptr);
void setStateA(APtr aptr, int arg);
void freeA(APtr aptr);

#ifdef __cplusplus
}
#endif

#endif  // !__ACPP_CAPI_H__