import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

if nprocs <2:
    print("This program works only for at least two processes.")
    sys.exit()

message=42.*np.ones(1)*(rank)

# Second case
if rank==0:
    comm.Send([message,MPI.DOUBLE],dest=nprocs-1)
    comm.Recv([message,MPI.DOUBLE],source=1)

elif rank==nprocs-1:
    comm.Send([message,MPI.DOUBLE],dest=nprocs-2)
    comm.Recv([message,MPI.DOUBLE],source=0)

else:
    comm.Send([message,MPI.DOUBLE],dest=rank-1)
    comm.Recv([message,MPI.DOUBLE],source=rank+1)

if (rank==0) : print("\nSecond Case")
for n in range(nprocs):
    comm.Barrier()
    if (n==rank): print(rank,42*(rank+1),message)
