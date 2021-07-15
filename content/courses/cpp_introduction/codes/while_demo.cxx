#include <iostream>

int main() {

    int x, y, z;

    x=-20;
    y=-10;

    while (x<0 && y<0) {
        x=10-y;
        y+=1;
        z=0;
    }
    z=1;

    std::cout<<x<<" "<<y<<" "<<z<<"\n";

}

