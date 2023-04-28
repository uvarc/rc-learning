#include <iostream>
#include <sstream>
#include <string>

int main() {

    std::string num_in;
    float number;

    do {
        std::cout<<"Please enter a number. -999 to stop.\n";
        std::cin>>num_in;
        std::stringstream ss(num_in);
        ss>>number;
        std::cout<<"You entered "<<number<<std::endl;
    } while (number!=-999);

    return 0;

}
    
