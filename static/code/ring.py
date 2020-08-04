from mpi4py import MPI
import numpy as np
import sys

comm=MPI.COMM_WORLD
my_rank=comm.Get_rank()
npes   =comm.Get_size()

if my_rank==0:
   baton=np.array([100],dtype='i')
else:
   baton=np.zeros((1,),dtype='i')

if npes==1:
   print 'Rank ',my_rank,'has the baton ',baton
   MPI.Finalize()
   sys.exit()

if my_rank==0:
   start=MPI.Wtime()
   comm.Send([baton,MPI.INT],1,0)
   comm.Recv([baton,MPI.INT],MPI.ANY_SOURCE,MPI.ANY_TAG)
   print 'Rank ',my_rank,'has the baton ',baton
   runtime=MPI.Wtime()-start
   print 'Total elapsed time was %.4f seconds'%runtime
else:
   comm.Recv([baton,MPI.INT],MPI.ANY_SOURCE,MPI.ANY_TAG)
   comm.Send([baton,MPI.INT],(my_rank+1)%npes,0)
   print 'Rank ',my_rank,'has the baton ',baton

MPI.Finalize()

