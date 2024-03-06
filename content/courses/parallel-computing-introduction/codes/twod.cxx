#include <iostream>

using namespace std;

int main (int argc, char *argv[]) {
  int i, j;

  int nrl=4;
  int ncl=4;

  int nrows=nrl+2;
  int ncols=ncl+2;
  double **u=new double*[nrows];  
  double *buf=new double[nrows];

  double *uptr=new double[(nrows)*(ncols)];

  for (i=0;i<nrows;++i,uptr+=ncols)
     u[i] = uptr;

  for (i=0;i<nrows;++i) {
     for (j=0;j<ncols;++j) {
        u[i][j]=i+j*2;
     }
  }

  for (i=0;i<nrows;++i) {
     buf[i]=u[i%ncols][0];
  }

  //Make sure it's right
  for (i=0;i<nrows;++i) {
     for (j=0;j<ncols;++j) {
        cout<<u[i][j]<<" "; 
     }
     cout<<"\n";
  }

  cout<<"Buffer\n";
  for (i=0;i<nrows;++i) {
     buf[i]=u[0][i*ncols];
     cout<<buf[i]<<" ";
  }
  cout<<"\n";

}


