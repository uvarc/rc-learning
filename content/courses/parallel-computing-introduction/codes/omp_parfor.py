from omp4py import *
import os

@omp
def who_runs_it():
    N=40
    nthreads=4
    with omp("parallel for num_threads(nthreads)"):
        for i in range(N):
            tid=omp_get_thread_num()
            print(f"Thread {tid} runs {i}")

who_runs_it()
