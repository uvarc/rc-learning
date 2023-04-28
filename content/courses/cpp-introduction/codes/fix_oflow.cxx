#include <iostream>
#include <cstring>

int main() {
    char greeting[6]="Hello";
    char musical_instr[6]="Cello";
    char str[5];
    int  year=2021;

    std::cout<<"sizeof: "<<sizeof(greeting)<<" "<<sizeof(musical_instr)<<"\n";
    std::cout<<"Initial value of year: "<<year<<"\n";
    strncpy(str,greeting,sizeof(str)-1);
    str[strlen(str)]='\0';
    std::cout<<"Str: "<<str<<"\n";
    std::cout<<"Year: "<<year<<"\n";

}
