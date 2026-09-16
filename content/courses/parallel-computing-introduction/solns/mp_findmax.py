import multiprocessing as mp
import time
import numpy as np
import os
def surface(coords): 
   """This is the main processing function."""
   x,y=coords
   mu1=np.sqrt(2.0)
   mu2=np.sqrt(np.pi)
   sig1=3.1
   sig2=1.4
   z1=0.1*np.sin(x)*np.sin(x*y)
   a=(x-mu1)**2/(2*sig1**2)
   b=(y-mu2)**2/(2*sig2**2)
   z2=np.exp(-(a+b))/(sig1*sig2*np.sqrt(2.0*np.pi))
   z=z1+z2
   return z.max()

if __name__ == '__main__':
   ncpus=int(os.getenv('NUM_PROCS'))
   nsamps=4000000
   # Define the parameters to test
   xlo=-10.*np.pi; xhi=10.*np.pi
   ylo=-10.*np.pi; yhi=10.*np.pi
   xyvals=np.random.uniform(xlo,xhi,size=(2,nsamps))
   coords=np.array_split(xyvals,ncpus,axis=1)
   pool = mp.Pool(processes=ncpus) 
   tic=time.time ()
   results = np.asarray(pool.map(surface, coords))
   print("Result is "+str(results.max()))
   toc=time.time ()
   print("Parallel time on "+str(ncpus)+" cores:"+str(round(toc-tic,4)))
   pool.close(); pool.join()

   tic=time.time()
   results=np.asarray(list(map(surface,coords)))
   print("Result is "+str(results.max()))
   toc=time.time()
   print("Serial time:"+str(round(toc-tic,4)))
   

