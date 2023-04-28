import sys
import numpy as np
from mpi4py import MPI

#Run on two processes

comm=MPI.COMM_WORLD
rank=comm.Get_rank()
nprocs=comm.Get_size()

x=None
a=np.empty(11,dtype='float')

if rank==0:
   x=10
   a=np.arange(11.,dtype='float')
   comm.send(x,dest=1)
   comm.Send([a,MPI.FLOAT],dest=1)
else:
   x=comm.recv(source=0)
   comm.Recv([a,MPI.FLOAT],source=0)

print(rank,x)
print(rank,a)
