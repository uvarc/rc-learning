/*
*testclass.cxx*
*/

class MyClass{
    public:
        double var1, var2;
        int var3;
        MyClass(double, double, int);
        ~MyClass();
    private:
        double privatevar;
};

MyClass::MyClass(double v1, double v2,int v3){
    var1=v1;
    var2=v2;
    var3=v3;
}
MyClass::~MyClass(){}

#include <iostream>

int main(int argc, char **argv){
    MyClass mytest(5.,6.,7);
    mytest.var1=11.; mytest.var2=25.; mytest.var3=5;
    mytest.privatevar=13.;  //ILLEGAL
    return 0;
}

