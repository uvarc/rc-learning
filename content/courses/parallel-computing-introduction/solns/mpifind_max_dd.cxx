#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>
#include <random>
#include <cmath>
#include "mpi.h"

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

   int nprocs, rank;

   MPI_Init(&argc, &argv);
   MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
   MPI_Comm_rank(MPI_COMM_WORLD, &rank);

   // For this example we'll use perfect square nprocs.  Otherwise we have
   // to tell it how to divide up the processes into rows and columns.
   float rows=pow(nprocs,0.5);
   int nrows=int(rows);
   int ncols=nrows;

   int nsamps;
   bool shutdown=false;

   if (rank==0) {
      if ( argc != 2 ) {
         cout <<"Usage: number of samples to test\n";
         shutdown=true;
      }
      else if ( pow(int(rows+0.5),2) != nprocs ) {
         cout << "Number of processes must be a perfect square\n";
	 shutdown=true;
      }
      else {
         nsamps=stoi(argv[1]);
      }
   }

   MPI_Bcast(&shutdown,1,MPI_CXX_BOOL,0,MPI_COMM_WORLD);
   if (shutdown) {
        MPI_Finalize();
        exit(1);
   }

   MPI_Bcast(&nsamps,1,MPI_INT,0,MPI_COMM_WORLD);

   // Define the parameters to test
   float pi=4.0*atan(1.0);

   float xlo=-10.*pi; 
   float xhi=10.*pi;
   float ylo=-10.*pi; 
   float yhi=10.*pi;

   float x_subrange=(xhi-xlo)/ncols;
   float y_subrange=(yhi-ylo)/nrows;

   // Calculate the range over which I will be working
   int col_num=rank%ncols;
   int row_num=rank/nrows;
   float my_xlo=xlo+col_num*x_subrange;
   float my_xhi=my_xlo+x_subrange;
   float my_ylo=ylo+row_num*y_subrange;
   float my_yhi=my_ylo+y_subrange;

   srand(time(0)+rank);

   float xmax=0.;
   float ymax=0.;
   float zmax=0.;

   for (int i=0;i<nsamps;++i) {
      for (int j=0;j<nsamps;++j) {
        xval=my_xlo+(my_xhi-my_xlo)*urand;
        yval=my_ylo+(my_yhi-my_ylo)*urand;
        zval=surface(xval,yval);
        if (zval>zmax) {
           zmax=zval;
           xmax=xval;
           ymax=yval;
	}
      }
   }

   float *xresult=new float[nprocs];
   float *yresult=new float[nprocs];
   float *zresult=new float[nprocs];

   MPI_Gather(&xmax,1,MPI_FLOAT,xresult,1,MPI_FLOAT,0,MPI_COMM_WORLD);
   MPI_Gather(&ymax,1,MPI_FLOAT,yresult,1,MPI_FLOAT,0,MPI_COMM_WORLD);
   MPI_Gather(&zmax,1,MPI_FLOAT,zresult,1,MPI_FLOAT,0,MPI_COMM_WORLD);

   if (rank==0) {
       float g_zmax=zresult[0];
       float g_xmax=xresult[0];
       float g_ymax=yresult[0];
       for (int i=1;i<nprocs;++i) {
           if (zresult[i] > g_zmax) {
               g_zmax=zresult[i];
	       g_xmax=xresult[i];
	       g_ymax=yresult[i];
	   }
       }
       cout<<"Result is "<<g_zmax<<" at x,y="<<g_xmax<<", "<<g_ymax<<endl;
   }

   MPI_Finalize();

}
