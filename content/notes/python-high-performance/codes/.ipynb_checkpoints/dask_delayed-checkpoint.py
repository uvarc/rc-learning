import dask
from dask.distributed import Client, progress
import time
import random

def inc(x):
    time.sleep(random.random())
    return x + 1

def dec(x):
    time.sleep(random.random())
    return x - 1

def add(x, y):
    time.sleep(random.random())
    return x + y

if __name__=="__main__":
   client = Client(threads_per_worker=4, n_workers=1)

   zs=[]
   for i in range(20):
       x = dask.delayed(inc(i))
       y = dask.delayed(dec(x))
       z = dask.delayed(add(x, y))
       z=z.compute()
       zs.append(z)
   print(zs)

   client.close()

