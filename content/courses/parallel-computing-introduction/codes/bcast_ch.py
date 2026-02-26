import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

if rank==0:
    message=np.array(["I have a secret"],dtype='str')
else:
    message=np.array(["               "],dtype='str')

comm.Bcast([message,MPI.CHAR])

print(rank,message[0])
