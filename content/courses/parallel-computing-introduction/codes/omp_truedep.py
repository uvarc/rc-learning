import numpy as np
from omp4py import *

@omp
def output_dependency(N):
    a=np.zeros(N,dtype='int')
    a[0]=100;

    with omp("parallel for"):
        for i in range(1,N):
            a[i]=a[i-1]+10

    return a

N=48;
a=output_dependency(N)
for i in range(N):
    print(i,a[i])
