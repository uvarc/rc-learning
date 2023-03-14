#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>
#include <random>
#include <cmath>

using namespace std;

#define urand ((double)rand()/(double)RAND_MAX);

float surface(float x, float y) {
   float pi=4.0*atan(1.0);
   float mu1, mu2, sig1, sig2;
   float a, b;
   float z1, z2;

   mu1=sqrt(2.0);
   mu2=sqrt(pi);
   sig1=3.1;
   sig2=1.4;
   z1=0.1*sin(x)*sin(x*y);
   a=pow((x-mu1),2)/(2*pow(sig1,2));
   b=pow((y-mu2),2)/(2*pow(sig2,2));
   z2=exp(-(a+b))/(sig1*sig2*sqrt(2.0*pi));
   return z1+z2;
}

int main(int argc, char **argv) {

   float xval, yval, zval;

   int nsamps;
   if ( argc != 2 ) {
      cout <<"Usage: number of samples to test\n";
      return 1;
   }
   else {
      nsamps=stoi(argv[1]);
   }

   // Define the parameters to test
   float pi=4.0*atan(1.0);

   float xlo=-10.*pi; 
   float xhi=10.*pi;
   float ylo=-10.*pi; 
   float yhi=10.*pi;

   srand(time(0));

   float xmax=0.;
   float ymax=0.;
   float zmax=0.;

   for (int i=0;i<nsamps;++i) {
      for (int j=0;j<nsamps;++j) {
        xval=xlo+(xhi-xlo)*urand;
        yval=ylo+(yhi-ylo)*urand;
        zval=surface(xval,yval);
        if (zval>zmax) {
           zmax=zval;
           xmax=xval;
           ymax=yval;
	}
      }
   }

   cout<<"Result is "<<zmax<<" at "<<xmax<<","<<ymax<<endl;

}
