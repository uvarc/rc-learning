from mpi4py import MPI
import numpy as np
import os
def surface(x,y): 
   """This is the main processing function.  We use a ufunc for MPI."""
   mu1=np.sqrt(2.0)
   mu2=np.sqrt(np.pi)
   sig1=3.1
   sig2=1.4
   z1=0.1*np.sin(x)*np.sin(x*y)
   a=(x-mu1)**2/(2*sig1**2)
   b=(y-mu2)**2/(2*sig2**2)
   z2=np.exp(-(a+b))/(sig1*sig2*np.sqrt(2.0*np.pi))
   z=z1+z2
   return z

if __name__ == '__main__':

   nsamps=4000000  #global number to be divided
   
   comm=MPI.COMM_WORLD
   rank=comm.Get_rank()
   nprocs=comm.Get_size()

   my_nsamps=nsamps/nprocs
   if nsamps%nprocs !=0:
      #somewhat dopey load-balancing
      remainder=nsamps%nprocs
      for n in range(remainder):
          if rank==n:
             my_nsamps+=1
           

   # Define the parameters to test
   xlo=-10.*np.pi; xhi=10.*np.pi
   ylo=-10.*np.pi; yhi=10.*np.pi
   xvals=np.random.uniform(xlo,xhi,my_nsamps)
   yvals=np.random.uniform(ylo,yhi,my_nsamps)
   tic=MPI.Wtime()
   z=surface(xvals,yvals)
   my_max=np.array([z.max()])
   my_max_i=z.argmax()
   my_max_x=xvals[my_max_i]
   my_max_y=yvals[my_max_i]
   xresult=np.empty(nprocs)
   yresult=np.empty(nprocs)
   zresult=np.empty(nprocs)
   comm.Gather([my_max_x,MPI.DOUBLE],[xresult,MPI.DOUBLE],root=0)
   comm.Gather([my_max_y,MPI.DOUBLE],[yresult,MPI.DOUBLE],root=0)
   comm.Gather([my_max,MPI.DOUBLE],[zresult,MPI.DOUBLE],root=0)
   if rank==0:
      result=zresult.max()
      max_i=zresult.argmax()
      x=xresult[max_i]
      y=yresult[max_i]
   toc=MPI.Wtime()
   if rank==0:
      print "Result on %d cores (x,y,z):%.4f,%.4f,%.4f"%(nprocs,x,y,result)
      print "Parallel time "+str(toc-tic)
