import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

if nprocs%2 != 0:
    print("This program works only for an even number of processes.")
    sys.exit()

message=np.zeros(1,dtype='int')
rank_val=rank*np.ones(1,dtype='int')

half = nprocs//2
if rank < half:
    partner=half+rank
else:
    partner=rank-half

if rank<half:
    comm.Recv([message,MPI.INT],source=partner)
    comm.Send([rank_val,MPI.INT],dest=partner)
else:
    comm.Send([rank_val,MPI.INT],dest=partner)
    comm.Recv([message,MPI.INT],source=partner)

print(rank,message)
