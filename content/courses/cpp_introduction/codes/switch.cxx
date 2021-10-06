#include <iostream>

int main() {

    float x1,x2,y;
    int chooser;

    x1=11.53;
    x2=57.9;

    if (x1<0.) {
       chooser=-1;
    } else if (x1==0.) {
         chooser=0;
    } else {
         chooser=1;
    }

    switch (chooser) {
        case  (-1):
             y=-x2;
             break;
        case (0):
             y=0.;
             break;
        default:
             y=x2+3./x1;
    }

    std::cout<<"Y is "<<y<<"\n";
}



