/*
 * twod.cxx
 */ 

#include <iostream>
using namespace std;

int main() {

  float **A;
  int nRows=10;
  int nCols=12;
  A=new float*[nRows];
  for (int n=0;n<nRows;++n){
    A[n]=new float[nCols];
  }
  
  for (int i=0;i<nRows;++i) {
      for (int j=0;j<nCols;++j) {
           A[i][j]=i+j;
      }
  }

  cout<<A[3][4]<<endl;

  return 0;
}

