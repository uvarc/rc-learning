/* Computes pi by a Monte-Carlo method
 *
*/

#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>
#include <random>
#include <cmath>

using namespace std;

#define urand ((double)rand()/(double)RAND_MAX);

long long throwDarts(long long nThrows) {

   long long hits=0;
   for (int i=0; i<nThrows; ++i) {
       double x = urand;
       double y = urand;
       if ( sqrt(pow(x,2)+pow(y,2)) < 1.0 ) {
           hits++;
       }
   }
   return hits;
}

int main(int argc, char **argv) {

   long long nThrows;

   if (argc != 2) {
      cout<<"Usage: nThrows\n";
      return 0;
   }
   else {
      string steps=argv[1];
      stringstream ssteps;
      ssteps<<steps;
      ssteps>>nThrows;
   }

   //It's better to use C++ random-number generators, but that requires some
   //additional coding, and possibly templating, so let's keep it simple and
   //fast for this example.
   srand(time(0));


   long long nHits=throwDarts(nThrows);

   double pi=4.0*(double)nHits/(double)nThrows;

   cout<<"Pi is "<<setprecision(6)<<pi<<endl;

   return 0;

}
