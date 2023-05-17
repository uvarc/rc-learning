#include <cmath>
#include <iostream>

using namespace std;

float mean(float *A, int n) {
    
    float sum=0.;
    for (int i=0;i<n;++i) {
        sum+=A[i];
    }
    return (sum/float(n));
}

float stdv(float *A, int n) {
     
    float mean_A=mean(A,n);
    float sumsqr=0.;
    for (int i=0;i<n;++i) {
         sumsqr+=pow(A[i]-mean_A,2);
    }
    return sqrt(sumsqr/n);
}

int reject_outliers(float *A, bool* mask, int n) {
     
    float dev, prob;

    float mean_A=mean(A,n);
    float stdv_A=stdv(A,n);
    float criterion=1.0/(2.*n);
    float sqrt2=sqrt(2.0);
    int numRejected=0;
    for (int i=0; i<n; ++i) {
        dev=abs(A[i]-mean_A)/stdv_A;
        dev/=sqrt2;
        prob=erfc(dev);
        if (prob>=criterion) {
           mask[i]=true;
        }
        else {
           mask[i]=false;
           numRejected++;
        }
   }
   return numRejected;
}
     

