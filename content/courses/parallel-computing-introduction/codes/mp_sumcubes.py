import numpy as np
import os
from multiprocessing import Pool

def f(x):
    return x**3

if __name__ == '__main__':

    ncpus=int(os.getenv('NUM_PROCS'))

    pool = Pool(processes=ncpus)
    result = pool.map(f,np.arange(1.,1000.,.1))
    print(sum(result))
    pool.close()
    pool.join()

