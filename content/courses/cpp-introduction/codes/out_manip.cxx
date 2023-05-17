#include <iostream>
#include <iomanip>

int main() {

   float x=.00001, y=17., z=10000.;

   std::cout<<std::setprecision(16)<<z/y<<"\n";
   std::cout<<std::setw(20)<<std::setfill('*')<<std::left<<z<<"\n";
   std::cout<<std::scientific<<x<<" "<<z<<"\n";
   std::cout<<std::scientific<<x<<" "<<std::fixed<<z<<"\n";

   return 0;
}

