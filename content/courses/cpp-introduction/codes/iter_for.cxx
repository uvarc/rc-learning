#include <iostream>
#include <string>

int main() {

   std::string greeting="Hello Everybody";

   for (char s :greeting) {
       std::cout<<s<<" ";
   }

   std::cout<<"\n";

   return 0;

}
