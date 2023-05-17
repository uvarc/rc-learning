import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

if nprocs !=2:
    print("This program works only for two processes.")
    sys.exit()

baton=np.empty(1)

if rank==0:
    comm.Recv([baton,MPI.DOUBLE],source=1)
    baton+=1.
    comm.Send([baton,MPI.DOUBLE],dest=1)

elif rank==1:
    baton[:]=12.
    comm.Send([baton,MPI.DOUBLE],dest=0)
    comm.Recv([baton,MPI.DOUBLE],source=0)

print(rank,baton)
