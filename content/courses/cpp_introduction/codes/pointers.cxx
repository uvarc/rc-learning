#include <iostream>

int main() {
/* Pointers
*/
    float *x;
    float y;
    y=99.;
    x=&y;

    std::cout<<"x:"<<x<<" y:"<<y<<" &y:"<<&y<<" *x:"<<*x<<"\n";

    return 0;
}
