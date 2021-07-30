#include <iostream>

int passme(int, float &);

int main() {

    int i=12;
    float x=11.4;

    std::cout<<"before i="<<i<<" x="<<x<<"\n";
    int j=passme(i,x);
    std::cout<<"after function result="<<j<<" i="<<i<<" x="<<x<<"\n";

    return 0;
}

int passme(int i, float &x) {

    x=x+3.9;
    i=int(x);
    std::cout<<"in function i="<<i<<" x="<<x<<"\n";

    return i+3;

}
