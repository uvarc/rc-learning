#include <iostream>
#include "adder.h"

int main() {
    float x,y;

    x=4.3; y=11.7;
    float res=adder(x,y);
    std::cout<<res<<"\n";

    return 0;
}
