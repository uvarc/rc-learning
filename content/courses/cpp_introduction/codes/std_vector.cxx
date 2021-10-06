#include <iostream>
#include <vector>

int main() {

    size_t N=5;
    //Initializer list (C++11)
    std::vector<int> x={1,2,3,4,5};

    for (int element : x) {
        std::cout<<element<<" ";
    }
    std::cout<<"\n";

    //Loop over a predefined size
    std::vector<float> y(N);
    for (std::size_t i=0; i<y.size(); ++i) {
        y[i]=float(i)-1.;
        std::cout<<y[i]<<" ";
    }
    std::cout<<"\n";

    //Dynamically append
    std::vector<float> z={};
    for (std::size_t i=0; i<N; ++i) {
        z.push_back(float(i)+2.);
    }
    for (std::size_t i=0; i<z.size(); ++i) {
        std::cout<<z[i]<<" ";
    }
    std::cout<<"\n";

    return 0;
}
