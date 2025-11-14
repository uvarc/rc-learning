#include <iostream>
#include <cmath>
#include <string>
#include <sstream>
#include <fstream>
#include <iomanip>

#define MAX_ITER 10000000

void set_bcs(double **u, int nr, int nc);

using namespace std; 

int main (int argc, char *argv[]) {
  int i, j;
  double diff;            // Change in value 
  double mean;            //Average boundary value 
  int N, M;
  int iterations = 0;
  int diffInterval;
  double epsilon;

/* check number of parameters and read in epsilon, filename, optionally size */
  if (argc < 3) {
    printf ("USAGE:  %s epsilon output-file <N> <M>\n", argv[0]);
    return 1;
  }
  else {
     stringstream ssepsilon;
     ssepsilon<<argv[1];
     if (ssepsilon.fail()) {
         printf ("Error reading in epsilon value\n");
         return 2;
     }
     ssepsilon>>epsilon;

     if (argc==3) {
        N=500;
	M=500;
     }
     if (argc==4) {
        stringstream ssnr;
	ssnr<<argv[3];
        if (ssnr.fail()) {
            printf ("Error converting row dimension \n");
            return 2;
	}
        ssnr>>N;
	M=N;
     }
     if (argc==5) {
        stringstream ssnr;
	ssnr<<argv[3];
        if (ssnr.fail()) {
            printf ("Error converting row dimension \n");
            return 2;
	}
        ssnr>>N;
	stringstream ssnc;
	ssnc<<argv[4];
        if (ssnc.fail()) {
            printf ("Error converting row dimension \n");
            return 2;
	}
        ssnc>>M;
     }
  }

  cout<<"Running until the difference is <="<<epsilon<<" with size "<<N<<"x"<<M<<"\n";

  int nr=N;
  int nc=M;

  int nrows=nr+2;
  int ncols=nc+2;
  double **u=new double*[nrows];  // Old values
  double **w=new double*[nrows];  // New values
  double **diffs=new double*[nrows+1];  // Diffs

  double *uptr=new double[(nrows)*(ncols)];
  double *wptr=new double[(nrows)*(ncols)];
  double *dptr=new double[(nrows)*(ncols)];

  for (i=0;i<nrows;++i,uptr+=ncols)
     u[i] = uptr;
  for (i=0;i<nrows;++i,wptr+=ncols)
     w[i] = wptr;
  for (i=0;i<nrows;++i,dptr+=ncols)
     diffs[i] = dptr;

  mean = 0.0;

  set_bcs(u, nr, nc);

  //Initialize to something like the mean boundary. Not important, can use zeros
  //but this may speed convergence a bit.
  for ( i = 1; i < nr+1; i++ ) {
    mean += u[i][0] + u[i][nc+1] + u[0][i] + u[nr+1][i];
  }
  mean /= (4.0 * (nr+2));

  // Initialize interior values
  for ( i = 1; i <= nr; i++ ) {
      for (j = 1; j <= nc; j++ ) {
          u[i][j] = mean;
      }
  }

  //Arbitrary, just to make sure we enter loop
   diff = 10.*epsilon;

  diffInterval=1;

  time_t time=clock();

  // Compute steady-state solution 
  // We are making a tradeoff between memory and speed here by creating a diffs
  // array. Especially for parallel, this should be reasonable.

  while ( diff >= epsilon) {
     diff=.8*epsilon;  // reset each time through to get max abs diff
     for (i=1; i<=nr;i++) {
        for (j=1;j<=nc;j++) {
            w[i][j] = (u[i-1][j] + u[i+1][j] + u[i][j-1] + u[i][j+1])/4.0;
     	    diffs[i][j] = abs(w[i][j] - u[i][j]);
         }
     }

     if (iterations%diffInterval==0) {
        for (i=1; i<=nr;i++) {
           for (j=1;j<=nc;j++) {
	       if (diff<diffs[i][j]) {
		   diff=diffs[i][j];
	       }
	   }
	 }
         if (diff <= epsilon) 
             break;
     }

     for (i=1; i<=nr;i++) {
        for (j=1;j<=nc;j++) {
            u[i][j] = w[i][j];
        }
     }

     if (iterations >= MAX_ITER) 
        break;

     iterations++;
  } 

  time_t totalTime=(clock()-time)/CLOCKS_PER_SEC;
  cout << "Completed "<<iterations<<" iterations; time "<<totalTime<<endl;
  // Write solution to output file
  ofstream fp;
  fp.open(argv[2],ios::out);
  for (i = 1; i <= nr; i++) {
     for (j = 1; j <= nc; j++) {
        fp<<u[i][j]<<" ";
     }
     fp<<"\n";
  }
  fp.close();

  // All done!
  cout<<"wrote output file "<<argv[2]<<"\n";
  return 0;
}

void set_bcs(double **u, int nr, int nc) {
  /* Set boundary values.
   * This has an ice bath on the top edge.
   * Note: when plotting, 0,0 is the bottom.
   */

  double topBC=0;
  double bottomBC=100.;
  double leftBC=100.;
  double rightBC=100.;

  for (int i=0;i<=nc+1;++i){
        u[0][i]=topBC;
  }
  for (int i=0;i<=nc+1;++i){
        u[nr+1][i]=bottomBC;
  }

  for (int i=1;i<=nr;++i){
      u[i][0]=leftBC;
      u[i][nc+1]=rightBC;
  }
}

