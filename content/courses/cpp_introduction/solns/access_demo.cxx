/*
*testclass.cxx*
*/

class MyClass{
    public:
        double var1, var2;
        int var3;
        MyClass(double, double, int);
        ~MyClass();
        void set_privatevar(double value);
        double get_privatevar();

    private:
        double privatevar;
};

MyClass::MyClass(double v1, double v2,int v3){
    var1=v1;var2=v2;var3=v3;
}

MyClass::~MyClass(){}

void MyClass::set_privatevar(double value) {
    privatevar=value;
    return;
}

double MyClass::get_privatevar(){
   return privatevar;
}


#include <iostream>
using namespace std;

int main(int argc, char **argv){
    MyClass mytest(5.,6.,7);
    mytest.var1=11.; mytest.var2=25.; mytest.var3=5;
    mytest.set_privatevar(13.);

    cout<<mytest.get_privatevar()<<"\n";

    return 0;
}

