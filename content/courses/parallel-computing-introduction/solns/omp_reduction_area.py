import os
from omp4py import *

@omp
def pie(nthreads):
    omp_set_num_threads(nthreads)
    n=1000
    area=0.0
    x=0.0
    with omp("parallel for private(x) reduction(+:area)"):
        for i in range(n):
            x=(i+0.5)/n;
            area+=4.0/(1.0+x*x);

    pi=area/n;
    return pi

nthreads=os.cpu_count()
pi=pie(nthreads)
print(f"Pi is {pi:.6f}")
