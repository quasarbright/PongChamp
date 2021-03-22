#include <iostream>
#include "acpp.h"

using namespace std;

A::A(int v): _v(v) {
    std::cout << "Construct A, value: " << _v << std::endl;
}

void A::applyState(i2i arg) {
    this->_v = arg(this->_v);
}

int A::getState() {
    return this->_v;
}

void A::setState(int arg) {
    this->_v = arg;
}

A::~A() {
    std::cout << "Destruct A" << std::endl;
}