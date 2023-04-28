#include <iostream>

int main() {
    double a,b;
    int c,n;

    a=11.; b=9.; c=45; n=3;

    std::cout<< (a>b) << "\n";
    std::cout<< (a<b && c==n) << "\n";
    std::cout<< (a<b || c==n) << "\n";
    std::cout<< (a>b || c==n && a<b) << "\n";
    std::cout<< ((a>b || c==n) && a<b) << "\n";
    bool is_equal=a==b;
    std::cout<< is_equal << "\n";

    return 0;
}
