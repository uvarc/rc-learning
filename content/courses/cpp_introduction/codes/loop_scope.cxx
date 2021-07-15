#include <iostream>

int main() {

   int i;

   for (i=0;i<10;++i) {
       std::cout<<"I is "<<i<<"\n";
   }

   std::cout<<"Outside the loop I is "<<i<<"\n";

   for (int j=0;j<10;++j) {
       std::cout<<"J is "<<j<<"\n";
   }

   //std::cout<<"Outside the loop J is "<<j<<"\n";

}
