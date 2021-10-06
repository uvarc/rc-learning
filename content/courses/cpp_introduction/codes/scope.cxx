#include <iostream>

int j=99.;

int mysub(int, float);

int main() {

    int i=12;
    float x=11.4;

    int j=mysub(i,x);
    std::cout<<"outside function i="<<i<<" j="<<x<<"\n";
    std::cout<<"global j "<<::j<<"\n";

    //code block
    { 
    int i=19;
    std::cout<<"i in block "<<i<<"\n";
    }
    std::cout<<"i outside block "<<i<<"\n";

    return 0;
}

int mysub(int i, float x) {

    x=x+3.9;
    i=int(x);
    std::cout<<"in function i="<<i<<" x="<<x<<" j="<<j<<"\n";

    return i+3;

}
