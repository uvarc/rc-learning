import numpy as np
import os
import concurrent.futures

def f(x):
    return x**3

if __name__ == '__main__':

    ncpus=int(os.getenv('NUM_PROCS'))

    with concurrent.futures.ProcessPoolExecutor(max_workers=ncpus) as executor:
        numbers=np.arange(1.,1000.,.1)
        result=executor.map(f, numbers)
    print(sum(result))

