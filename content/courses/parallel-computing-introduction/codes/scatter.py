import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

if 100%nprocs!=0:
    if rank==0:
        print("For this simple example, nprocs must evenly divide 100")
    sys.exit()
else:
    sendcount=100//nprocs

values=np.linspace(1.,100.,100)
myvals=np.empty(sendcount)

comm.Scatter([values,sendcount,MPI.DOUBLE],myvals)

print(rank,myvals)
