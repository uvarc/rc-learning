#include <iostream>

int main() {
/* My first program
   Author: My Name
   Date: Today
*/

    int i, j, k;

    i=2;

    j=i++;

    std::cout<<" i is: "<<i<<"\n";
    std::cout<<" j is: "<<j<<"\n";

    j=++i;
    std::cout<<" i is: "<<i<<"\n";
    std::cout<<" j is: "<<j<<"\n";
    
    
    return 0;
}
