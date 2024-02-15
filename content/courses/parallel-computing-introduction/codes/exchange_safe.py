import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

if nprocs<2:
    print("This program works only for at least two processes.")
    sys.exit()
elif nprocs%2!=0:
    print("This program works only for an even number of processes.")
    sys.exit()

message=np.zeros(1,dtype='int')
rank_val=rank*np.ones(1,dtype='int')

if rank%2==0:
    neighbor=rank+1
else:
    neighbor=rank-1

if rank%2==0:
    comm.Recv([message,MPI.INT],source=neighbor)
    comm.Send([rank_val,MPI.INT],dest=neighbor)
else:
    comm.Send([rank_val,MPI.INT],dest=neighbor)
    comm.Recv([message,MPI.INT],source=neighbor)

print(rank,message)
