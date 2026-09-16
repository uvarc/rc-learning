import numpy as np
from omp4py import *

@omp
def output_dependency(a,N):

    with omp("parallel for"):
        for i in range(N-1):
            a[i]=a[i+1]+10
    return a

N=48;
a=np.zeros(N,dtype='int')
for i in range(N):
    a[i]=100-i;

a=output_dependency(a,N)

for i in range(N):
    print(i,a[i])
