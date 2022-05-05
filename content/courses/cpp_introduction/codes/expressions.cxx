#include <iostream>

int main (int argc, char **argv) {
    double x,Xs;
    int num_1,num_2;

    x=17.; Xs=11.;
    num_1=10; num_2=14;

    std::cout<<x<<"\n";
    std::cout<<Xs/x<<"\n";
    std::cout<<(int)Xs/x<<"\n";
    std::cout<<int(Xs)/int(x)<<"\n";
    std::cout<<Xs/x + x<<"\n";
    std::cout<<Xs/(x+x)<<"\n";
    std::cout<<x/num_1<<"\n";
    std::cout<<num_1/num_2<<"\n";
    std::cout<<num_2/num_1<<"\n";

    return 0;
}
