import numpy as np
from omp4py import *

@omp
def output_dependency(N):
    a=np.zeros(N,dtype='int')
    x=5;
    a[N-1]=100;

    with omp("parallel for"):
        for i in range(N-1):
            a[i]=i
            a[i+1]=x+i

    return a

N=48;
a=output_dependency(N)
for i in range(N):
    print(i,a[i])
