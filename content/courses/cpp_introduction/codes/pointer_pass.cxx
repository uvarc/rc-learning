#include <iostream>

void passme(int *, float &);

int main() {

    int *a;
    float x=11.4;

    a=new int(4);
    for (int i=0; i<4; ++i) {
        a[i]=i;
    }

    passme(a,x);

    for (int i=0; i<4; ++i) {
        std::cout<<a[i]<<" ";
    }
    std::cout<<"\n";
    std::cout<<"x is now "<<x<<"\n";

    return 0;
}

void passme(int *a, float &x) {

    x=x+3.9;
    for (int i=0; i<4; ++i) {
        a[i]=i*3+int(x);
    }
}
