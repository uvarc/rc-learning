#include <iostream>
#include <cstring>

int main() {
    char greeting[6]="Hello";
    char musical_instr[6]="Cello";
    char two_strings[13]="";

    std::cout<<strcmp(greeting,musical_instr)<<"\n";
    std::cout<<strcat(two_strings,greeting)<<"\n";
    std::cout<<strcat(two_strings,musical_instr)<<"\n";
    std::cout<<strlen(greeting)<<"\n";
    std::cout<<strcat(greeting,musical_instr)<<"\n";
    std::cout<<greeting<<"\n";
    std::cout<<strlen(greeting)<<"\n";

    char str[6];
    strcpy(str,greeting);
    std::cout<<str<<"\n";
    
}
