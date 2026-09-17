from omp4py import *
import os

def set_init_omp_nthreads():
    global _threads
    if os.getenv("OMP_NUM_THREADS") is not None:
        _threads=int(os.getenv("OMP_NUM_THREADS"))
    else:
        _threads=os.cpu_count()
    omp_set_num_threads(_threads)

@omp
def hello():
    tid=0
    with omp("parallel private(tid)"):
        tid=omp_get_thread_num()
        print(f"Hello from thread {tid} of {_threads}")
    return None

set_init_omp_nthreads()
hello()
