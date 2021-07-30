#include <iostream>
#include <array>

int main() {

    const int N=5;
    std::array<int,N> x={1,2,3,4,5};

    for (int element : x) {
        std::cout<<element<<" ";
    }
    std::cout<<"\n";

    for (std::size_t i=0; i<x.size(); ++i) {
        x[i]+=2;
    }

    return 0;
}
