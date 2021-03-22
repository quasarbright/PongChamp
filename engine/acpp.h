#ifndef __ACPP_H__
#define __ACPP_H__

#ifdef __cplusplus

typedef int (i2i)(int);

class A {
   public:
    A(int v);
    void applyState(i2i arg);
    int getState();
    void setState(int arg);
    ~A();

   public:
    int _v;
};

#endif // !__cplusplus

#endif  // !__ACPP_H__