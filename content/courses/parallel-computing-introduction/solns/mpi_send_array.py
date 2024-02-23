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

nelems=10

u=np.zeros(nelems)
w=np.zeros(nelems)

for i in range(nelems):
    u[i]=20.+i*rank

if rank%2==0:
    neighbor=rank+1
else:
    neighbor=rank-1

comm.Sendrecv([u,MPI.DOUBLE],neighbor,0,[w,MPI.DOUBLE],neighbor,0,MPI.Status())

for i in range(nelems):
    print(f"{rank}  {i}  {u[i]}  {w[i]}")
