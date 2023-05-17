import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

myvals=np.linspace(rank,rank+11,1)
sendcount=10

values=np.empty(sendcount*10)

comm.Scatter([values,sendcount,MPI.DOUBLE],myvals)

print(rank,myvals)
