import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

myvals=(rank+1)*np.linspace(1.,10.,10)
print(rank,myvals)
sendcount=myvals.size

allvals=np.zeros(sendcount*nprocs)
comm.Gather([myvals,MPI.DOUBLE],[allvals,MPI.DOUBLE])

if (rank==0):
    print(allvals)
