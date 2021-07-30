#include <cstdlib>
#include <iostream>

int main() {

    int   N=10;

    float *x;
    for (int i=1;i<=5;++i) {
        if (x != NULL) delete x;
        x=(float *) new float[N];
        std::cout<<"New:"<<x<<"\n";
    }

    return 0;
}
