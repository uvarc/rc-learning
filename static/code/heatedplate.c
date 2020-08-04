/* Sequential Solution to Steady-State Heat Problem */
/* This code based on Quinn, page 331 */

#include <stdio.h>
#include <math.h>

/* if your N is to large, it will have problems allocating the necessary memory */
#define N 500

/* The sequential version of this program needs approximately
 * 18/epsilon iterations to complete. */

int main (int argc, char *argv[]) {
  double diff;        /* Change in value */
  int i, j;
  double mean;        /*Average boundary value */
  double u[N][N];     /* Old values */
  double w[N][N];     /* New values */
  int iterations = 0;
  FILE *fp;
  float epsilon;

  /* check number of parameters and read in epsilon */
  if (argc != 3) {
    printf ("USAGE:  %s <epsilon> <output-file>\n", argv[0]);
    return 1;
  }
  if ( sscanf (argv[1], "%f", &epsilon) != 1 ) {
    printf ("Error reading in epsilon value\n");
    return 2;
  }
  printf ("running until the difference is <= %f\n", epsilon);
  diff = epsilon;

  /* Set boundary values and compute mean boundary value.  Unlike the
   * book code, this has the ice bath on the top edge, not the bottom
   * edge. */
  mean = 0.0;
  for ( i = 0; i < N; i++ ) {
    u[i][0] = u[i][N-1] = u[N-1][i] = 100.0;
    u[0][i] = 0.0;
    mean += u[i][0] + u[i][N-1] + u[0][i] + u[N-1][i];
  }
  mean /= (4.0 * N);

  /* Initialize interior values */
  for ( i = 1; i < N-1; i++ )
    for (j = 1; j < N-1; j++ )
      u[i][j] = mean;

  /* Compute steady-state solution */
  while ( diff >= epsilon ) {
    diff = 0.0;
    for (i=1; i<N-1;i++)
      for (j=1;j<N-1;j++) {
	w[i][j] = (u[i-1][j] + u[i+1][j] +
		   u[i][j-1] + u[i][j+1])/4.0;
	if ( fabs(w[i][j] - u[i][j]) > diff )
	  diff = fabs(w[i][j] - u[i][j]);
      }
    if (diff >= epsilon) {
      for (i=1; i<N-1;i++) 
	for (j=1;j<N-1;j++)
	  u[i][j] = w[i][j];
      iterations++;
    }
  } 
  printf ("completed; iterations = %d\n", iterations);

  /* Write solution to output file */
  fp = fopen (argv[2], "w");
  fprintf (fp, "%d\n", N);
  for (i = 0; i < N; i++) {
    for (j = 0; j < N; j++)
      fprintf (fp, "%6.2f ", u[i][j]);
    fputc ('\n', fp);
  }
  fclose(fp);

  /* All done! */
  printf ("wrote output file '%s'\n", argv[2]);
}
