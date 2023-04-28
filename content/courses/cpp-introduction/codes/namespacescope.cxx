#include <iostream>
using namespace std;

namespace blue {
   float x, y;
}

namespace yellow {
   float x, y;
}

int main() {

   using blue::x;
   using yellow::y;

   x=10.4;
   blue::y=12.8;

   yellow::x=17.0;
   y=16.11;

   float z;
   z=x+yellow::x;
   std::cout<<"First result "<<z<<"\n";

   z=blue::y+y;
   std::cout<<"First result "<<z<<"\n";

   return 0;
}
