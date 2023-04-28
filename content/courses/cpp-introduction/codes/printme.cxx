#include <iostream>

void printme();

int main() {

    printme();

    return 0;
}

void printme() {

    std::cout<<"Mod 11,3 is "<<11%3<<"\n";
    std::cout<<"Mod -11,3 is "<<-11%3<<"\n";
    std::cout<<"Mod 11,-3 is "<<11%-3<<"\n";
}
