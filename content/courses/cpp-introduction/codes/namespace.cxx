#include <iostream>

namespace blue {
   float x, y;
}

namespace yellow {
   float x, y;
}

int main() {

   blue::x=10.4;
   blue::y=12.8;

   yellow::x=17.0;
   yellow::y=16.11;

   float z=blue::x+yellow::x;
   std::cout<<"Result "<<z<<"\n";

   return 0;
}