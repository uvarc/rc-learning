#include <iostream>

int main() {

   float x=.00001, y=17., z=10000.;

   printf ("floats: %15.8e %+.0e %E \n", x,y,z);
   printf ("Width trick: %*f \n", 5, x);

   return 0;
}

