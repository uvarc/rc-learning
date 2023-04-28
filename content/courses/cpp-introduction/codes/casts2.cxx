#include <iostream>
#include <iomanip>
using namespace std;

int main() {

   float r;
   double s;

   float o=1.;
   float t=3.;

   r=o/t;
   s=double(r);
   int i=int(t);

   cout<<setprecision(16)<<"Cast "<<s<<"\n";
   cout<<i<<"\n";
}
