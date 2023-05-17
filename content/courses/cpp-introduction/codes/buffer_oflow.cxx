#include <iostream>
#include <cstring>

int main() {
    char greeting[6]="Hello";
    char musical_instr[6]="Cello";
    char str[5];
    int  year=2021;

    std::cout<<"Initial value of year: "<<year<<"\n";
    strcat(greeting,musical_instr);
    strcpy(str,greeting);
    std::cout<<"What happened to year? "<<year<<"\n";

}
