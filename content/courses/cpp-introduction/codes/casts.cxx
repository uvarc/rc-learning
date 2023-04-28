#include <iostream>
#include <iomanip>
using namespace std;

int main() {

   float r;
   double s;

   float o=1.;
   float t=3.;

   r=o/t;
   s=(double)r;

   cout<<setprecision(16)<<"Cast "<<s<<"\n";
   cout<<setprecision(16)<<"Literals "<<1./3.<<"\n";
}
