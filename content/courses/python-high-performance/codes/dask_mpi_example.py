from distributed import Client, LocalCluster
import dask
import time
from dask_mpi import initialize
import random 

@dask.delayed
def inc(x):
    time.sleep(random.random())
    return x + 1

@dask.delayed
def dec(x):
    time.sleep(random.random())
    return x - 1

@dask.delayed
def add(x, y):
    time.sleep(random.random())
    return x + y

def main ():
   initialize(nanny=False)

   client = Client()
   zs = []
   for i in range(25):
      x = inc(i)
      y = dec(x)
      z = add(x, y)
      zs.append(z)

   result = dask.compute(*zs)
   print (result)


if __name__ == "__main__":
   main()

