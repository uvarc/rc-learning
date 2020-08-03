/* Sequential Solution to Steady-State Heat Problem */

#include <iostream>
#include <cmath>
#include <string>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <omp.h>

/* if your N is to large, it will have problems allocating the necessary memory */
#define MAX_ITER 100000000
#define N 500

using namespace std; 

int main (int argc, char *argv[]) {
  double diff;            // Change in value 
  int i, j;
  double mean;            //Average boundary value 
  double u[N+2][N+2];     // Old values 
  double w[N+2][N+2];     // New values
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


  time=omp_get_wtime();
  mean = 0.0;

#pragma omp parallel
{
#pragma omp master
  cout<<"Number of threads "<<omp_get_num_threads()<<endl;
#pragma omp parallel for reduction(+:mean)
  for ( i = 1; i < N+1; i++ ) {
    u[N+1][i] = topBC;
    u[i][0] = edgeBC;
    u[i][N+1] =edgeBC;
    u[0][i] = bottomBC;
    mean += u[i][0] + u[i][N+1] + u[0][i] + u[N+1][i];
  }
  mean /= (4.0 * N);

  // Initialize interior values
#pragma omp parallel for private(j)
  for ( i = 1; i <= N; i++ )
    for (j = 1; j <= N; j++ )
      u[i][j] = mean;
}

  // Compute steady-state solution 
  while ( diff >= epsilon && iterations<MAX_ITER ) {
    diff = 0.0;  //reset to get maximum
#pragma omp parallel for private(j) reduction(max:diff)
    for (i=1; i<=N;i++)
      for (j=1;j<=N;j++) {
        	w[i][j] = (u[i-1][j] + u[i+1][j] + u[i][j-1] + u[i][j+1])/4.0;
    	   if ( abs(w[i][j] - u[i][j]) >= diff )
	          diff = abs(w[i][j] - u[i][j]);
      }
      if (diff >= epsilon) {
#pragma omp parallel for private(j)
         for (i=1; i<=N;i++) 
  	         for (j=1;j<=N;j++)
         	  u[i][j] = w[i][j];
      }
      iterations++;
  } 

  cout << "Completed "<<iterations<<";time "<<omp_get_wtime()-time<<endl;
  // Write solution to output file
  ofstream fp;
  fp.open(argv[2],ios::out);
  fp<<N<<"\n";
  for (i = 1; i <= N; i++) {
    for (j = 1; j <= N; j++)
      fp<<u[i][j]<<" ";
    fp<<"\n";
  }
  fp.close();

  // All done!
  cout<<"wrote output file "<<argv[2]<<"\n";
}
