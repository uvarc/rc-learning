from omp4py import *

@omp
def private(nthreads):
    x=10
    with omp("parallel for private(x) num_threads(nthreads)"):
        for i in range(nthreads):
            tid=omp_get_thread_num()
            x=1000*(tid+1)
            print(f"Thread {tid} gets x={x} ")
    
    print(f"Outside parallel region x={x}")

@omp
def firstpriv(nthreads):
    y=20
    with omp("parallel for firstprivate(y) num_threads(nthreads)"):
        for i in range(nthreads):
            tid=omp_get_thread_num();
            w=y*100*(tid+1);
            print(f"Thread {tid} gets w={w} ")
    
    print(f"Outside parallel region y={y}")

"""
@omp
def lastpriv(nthreads):
    z=30
    with omp("parallel for lastprivate(y) num_threads(nthreads)"):
        for i in range(nthreads):
            tid=omp_get_thread_num();
            z=3000*(tid+1);
            print(f"Thread {tid} gets z={z}")
    
    print(f"Outside parallel region z= {z}")
"""


nthreads=4
private(nthreads)
firstpriv(nthreads)
#lastprivate not yet implemented, try later
#lastpriv(nthreads)

