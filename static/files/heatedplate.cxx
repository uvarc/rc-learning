/* Sequential Solution to Steady-State Heat Problem */

#include <iostream>
#include <cmath>
#include <string>
#include <sstream>
#include <fstream>
#include <iomanip>

/* if your N is to large, it will have problems allocating the necessary memory */
#define MAX_ITER 100000000

using namespace std; 

int main (int argc, char *argv[]) {
  double diff;            // Change in value 
  int i, j;
  double mean;            //Average boundary value 
  double **u;             // Old values 
  double **w;             // New values
  int N;
  int iterations = 0;
  double epsilon;
  float time, etime;

  /* check number of parameters and read in epsilon */
  if (argc != 3) {
    printf ("USAGE:  %s <epsilon> <output-file>\n", argv[0]);
    return 1;
  }
  stringstream ssepsilon;
  ssepsilon<<argv[1];
  if (ssepsilon.fail()) {
    printf ("Error reading in epsilon value\n");
    return 2;
  }

  ssepsilon>>epsilon;
  cout<<"running until the difference is <="<<epsilon<<"\n";
  diff = epsilon;

  /* Set boundary values and compute mean boundary value. 
   * This has an ice bath on the top edge.
   * Note: when plotting, 0,0 is the bottom.
   */
  double topBC=100;
  double bottomBC=0.;
  double edgeBC=100.;

  N=500;

  //The N+2 allows for two boundary zones in each dimension
  u=new double*[N+2];
  w=new double*[N+2];
  for (int i=0; i<N; ++i) {
     u[i]=new double[N+2];
     w[i]=new double[N+2];
  }

  mean = 0.0;
  for ( i = 1; i < N+1; i++ ) {
    u[N+1][i] = topBC;
    u[i][0] = edgeBC;
    u[i][N+1] =edgeBC;
    u[0][i] = bottomBC;
    mean += u[i][0] + u[i][N+1] + u[0][i] + u[N+1][i];
  }
  mean /= (4.0 * N);

  // Initialize interior values
  for ( i = 1; i <= N; i++ )
    for (j = 1; j <= N; j++ )
      u[i][j] = mean;

  time=clock();
  // Compute steady-state solution 
  while ( diff >= epsilon && iterations<MAX_ITER ) {
    diff = 0.0;  //reset to get maximum
    for (i=1; i<=N;i++)
      for (j=1;j<=N;j++) {
        	w[i][j] = (u[i-1][j] + u[i+1][j] + u[i][j-1] + u[i][j+1])/4.0;
    	   if ( abs(w[i][j] - u[i][j]) >= diff )
	          diff = abs(w[i][j] - u[i][j]);
      }
      if (diff >= epsilon) {
         for (i=1; i<=N;i++) 
  	         for (j=1;j<=N;j++)
         	  u[i][j] = w[i][j];
      }
      iterations++;
  } 

  cout << "Completed "<<iterations<<endl;
  // Write solution to output file
  ofstream fp;
  string fname=argv[2];
  fp.open(fname,ios::out);
  fp<<N<<"\n";
  for (i = 1; i <= N; i++) {
    for (j = 1; j <= N; j++)
      fp<<u[i][j]<<" ";
    fp<<"\n";
  }
  fp.close();

  // All done!
  cout<<"wrote output file "<<fname<<"\n";
}
