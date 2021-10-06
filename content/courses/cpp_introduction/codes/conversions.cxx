#include <iostream>
#include <iomanip>
#include <cmath>
using namespace std;

int main() {

   float r, x;
   double s;

   int i=-1100020;
   unsigned int j;
   j=i;
   cout<<"Signed to unsigned:"<<"\n"<<i<<"\n"<<j<<"\n";

   x=4.78;
   int k=x;
   cout<<setprecision(16)<<"Float to int:"<<"\n"<<x<<"\n"<<k<<"\n";

   s=acos(-1.0);
   r=s;

   cout<<setprecision(16)<<"Double to float:"<<"\n"<<s<<"\n"<<r<<"\n";

}
