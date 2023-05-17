import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

#Must declare space in all ranks.  Make sure the dtypes match.
values=np.empty(10,dtype='int')

if rank==0:
    values=np.arange(1,11,1,dtype='int')

comm.Bcast([values,MPI.INT])
print(rank,values)
