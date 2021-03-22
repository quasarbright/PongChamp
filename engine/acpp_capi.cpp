#include "acpp_capi.h"

APtr makeA(int a) {
    return new A(a);
}

void applyStateA(APtr aptr, i2i arg) {
    return aptr->applyState(arg);
}

int getStateA(APtr aptr) {
    return aptr->getState();
}

void setStateA(APtr aptr, int arg) {
    aptr->setState(arg);
}

void freeA(APtr aptr) {
    delete aptr;
}