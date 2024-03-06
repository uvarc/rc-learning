#include <iostream>
#include <cstring>

int main(int argc, char **argv) {
    char user[6];
    char password[8];

    std::cout<<"Enter your user id: ";
    std::cin>>user;
    std::cout<<"Enter your password: ";
    std::cin>>password;

    if (std::strcmp(password,"Eleventy")==0) {
        std::cout<<"You have logged in\n";
    }
    else {
        std::cout<<"Incorrect password\n";
    }
}
