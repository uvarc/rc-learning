import sys
import numpy as np
from mpi4py import MPI

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
nprocs = comm.Get_size()

N = 500

#Strong scaling
if N%nprocs==0:
    nrl = N//nprocs
else:
    print("Number of ranks should divide the number of rows evenly.")
    sys.exit()

#Weak scaling
#nrl = N

#Both
ncl = N

w = np.zeros((nrl+2, ncl+2), dtype=np.double)

#Set up boundaries
if rank == 0 :
    w[0,:] = 0.        # up

if rank == nprocs-1 :
    w[nrl+1,:] = 100.   # bottom

w[:,0] = 100.      # left
w[:,ncl+1] = 100.   # right

#Arbitarary value for interior that may speed up convergence somewhat.
#Be sure not to overwrite boundaries.
w[1:nrl+1,1:ncl+1] = 50.

# setting up the up and down rank for each process
if rank == 0 :
    up = MPI.PROC_NULL
else :
    up = rank - 1

if rank == nprocs - 1 :
    down = MPI.PROC_NULL
else :
    down = rank + 1

tag=0

# sending up and receiving down
comm.Sendrecv([w[1,1:ncl+1],MPI.DOUBLE], up, tag, [w[nrl+1,1:ncl+1],MPI.DOUBLE], down, tag)
# sending down and receiving up
comm.Sendrecv([w[nrl,1:ncl+1],MPI.DOUBLE], down, tag, [w[0,1:ncl+1],MPI.DOUBLE], up, tag)

# Spot-check result
for n in range(nprocs):
    if n==rank:
        print(n,w[0,ncl//2],w[nrl+1,ncl//2])
