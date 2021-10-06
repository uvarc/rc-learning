#include <iostream>

int myfunc(int, int=0, int=1 );

int main() {

    int i=7;
    std::cout<<myfunc(i)<<"\n";
    int j=5;
    std::cout<<myfunc(i,j)<<"\n";
    int k=3;
    std::cout<<myfunc(i,j,k)<<"\n";

    return 0;
}

int myfunc(int i, int j, int k) {
    return i+j+k;
}
