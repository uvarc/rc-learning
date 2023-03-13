#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>
#include <cmath>
//C++11
#include <functional>

using namespace std;

float f(float x) {
  return sin(x);
}

//Function pointer
//float trap(float a, float b, float h, int n, float (*function)(float)) {
//Templated function object
float trap(float a, float b, float h, int n, function<float(float)> f) {

   float integral = (f(a) + f(b))/2.0;
   float x=a;
   for (int i=0; i<n; ++i) {
       x+=h;
       integral += f(x);
   }

   integral *= h;
   return integral;
}


int main(int argc, char **argv) {
                                                                                
// Calculate a definite integral using trapezoid rule
                                                                                
    float  a, b;
    int    n;

    float  h, integral;
    float  x;

    if (argc != 4) {
        cout<<"Usage: lower bound, upper bound, number of steps\n";
        return 0;
    }
    else {
       stringstream lb;
       lb<<argv[1];
       lb>>a;
       a=stof(argv[1]);

       stringstream ub;
       ub<<argv[1];
       ub>>b;
       b=stof(argv[2]);

       stringstream steps;
       steps<<argv[3];
       steps>>n;
   }

   h=(b-a)/n;
   integral = trap(a,b,h,n,&f);

   cout<<" Result "<<integral<<endl;

}

