#include <iostream>

int main() {

    float w=11.;

    auto x=w;
    auto y=3.;
    auto z=3;
    
    std::cout<<typeid(x).name()<<" "<<typeid(y).name()<<" "<<typeid(z).name()<<"\n";
    
    return 0;
}
