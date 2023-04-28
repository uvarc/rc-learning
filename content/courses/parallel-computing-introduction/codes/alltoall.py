import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

mystart=rank*100.
myend=mystart+nprocs
myvals=np.arange(mystart,myend,1.)

# Receive buffer
recvals=np.zeros(myvals.size)

comm.Alltoall(myvals,recvals)

print(rank,recvals)
