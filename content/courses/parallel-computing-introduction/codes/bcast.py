import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

# Make sure the types match
if rank==0:
    message=np.array([42.])
else:
    message=np.array([0.])

comm.Bcast([message,MPI.DOUBLE])

print(rank,message[0])
