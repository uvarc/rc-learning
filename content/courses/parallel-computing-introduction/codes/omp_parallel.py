from omp4py import *

@omp
def hello():
    with omp("parallel"):
        tid=omp_get_thread_num()
        print(f"Hello from thread {tid}")
    
hello()
