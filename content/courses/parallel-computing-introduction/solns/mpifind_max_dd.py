from mpi4py import MPI
import numpy as np
import sys

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

   comm=MPI.COMM_WORLD
   rank=comm.Get_rank()
   nprocs=comm.Get_size()

   shutdown=None
   nsamps=0

   # For this example we'll use perfect square nprocs.  Otherwise we have
   # to tell it how to divide up the processes into rows and columns.
   nrows=np.sqrt(nprocs)
   ncols=nrows

   if rank==0:
       if int(ncols+0.5)**2 != nprocs:
           print("Number of processes must be a perfect square")
           shutdown=True
       elif len(sys.argv)==2:
           nsamps=int(float(sys.argv[1]))
           shutdown=False
       else:
           print("Usage: number of samples")
           shutdown=True

   shutdown=comm.bcast(shutdown,root=0)
   if shutdown:
       MPI.Finalize()

   nsamps=comm.bcast(nsamps,root=0)

   #Fairly crude effort to get different seeds on different processes
   rng = np.random.default_rng()

   # Define the parameters to test
   xlo=-10.*np.pi; xhi=10.*np.pi
   ylo=-10.*np.pi; yhi=10.*np.pi
   x_subrange=(xhi-xlo)/ncols
   y_subrange=(yhi-ylo)/nrows

   # Calculate the range over which I will be working
   col_num=rank%ncols
   row_num=rank//nrows
   my_xlo=xlo+col_num*x_subrange
   my_xhi=my_xlo+x_subrange
   my_ylo=ylo+row_num*y_subrange
   my_yhi=my_ylo+y_subrange

   tic=MPI.Wtime()

   #We would usually have a lot of x,y points so cannot create z as a function
   #of x and y, so have to use a loop
   zmax=0.
   for i in range(nsamps):
       for j in range(nsamps):
           xval=rng.uniform(my_xlo,my_xhi)
           yval=rng.uniform(my_ylo,my_yhi)
           zval=surface(xval,yval)
           if zval>zmax:
               zmax=zval
               xmax=xval
               ymax=yval
               max_xind=i
               max_yind=j

   my_xresult=np.array([xmax])
   my_yresult=np.array([ymax])
   my_zresult=np.array([zmax])

   xresult=np.empty(nprocs)
   yresult=np.empty(nprocs)
   zresult=np.empty(nprocs)

   comm.Gather([my_xresult,MPI.FLOAT],[xresult,MPI.FLOAT])
   comm.Gather([my_yresult,MPI.FLOAT],[yresult,MPI.FLOAT])
   comm.Gather([my_zresult,MPI.FLOAT],[zresult,MPI.FLOAT])

   toc=MPI.Wtime()

   if rank==0:
       g_zmax=zresult.max()
       maxind=np.argmax(zresult)
       g_xmax=xresult[maxind]
       g_ymax=yresult[maxind]
       print(f"Result is {g_zmax:.4f} at x,y= {g_xmax:.4f},{g_ymax:.4f} ")
   
   MPI.Finalize()
