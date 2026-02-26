import sys
import numpy as np
from mpi4py import MPI

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
nprocs = comm.Get_size()

N = 12

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

#Arbitarary value for interior
#Be sure not to overwrite boundaries.
w[1:nrl+1,1:ncl+1] = 50.

#Rank-specific values for some rows
w[1,:]=(rank+1)*2.
w[nrl,:]=(rank+1)*2.5

#Set up boundaries
topBC=0.
bottomBC=200.
leftBC=100.
rightBC=100.

if rank == 0 :
    w[0,:] = topBC     # up

if rank == nprocs-1 :
    w[nrl+1,:] = bottomBC  # bottom

w[:,0] = leftBC      # left
w[:,ncl+1] = rightBC   # right

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

# Receive
rcv_down=comm.Irecv([w[nrl+1,1:ncl+1],MPI.DOUBLE],down)
rcv_up=comm.Irecv([w[0,1:ncl+1],MPI.DOUBLE],up)

send_up=comm.Isend([w[1,1:ncl+1],MPI.DOUBLE], up)
send_down=comm.Isend([w[nrl,1:ncl+1],MPI.DOUBLE], down)

requests=[rcv_down,rcv_up,send_up,send_down]

MPI.Request.Waitall(requests)


# Print result.  We will skip printing the starting values this time.
comm.Barrier()
status=MPI.Status()
if rank==0:
    u=np.zeros_like(w)
    print("Final for rank 0")
    for i in range(nrl+2):
        print(w[i,:])
    print()

    for i in range(1,nprocs):
        comm.Recv([u,MPI.DOUBLE],source=MPI.ANY_SOURCE,tag=MPI.ANY_TAG,status=status)
        print("Final for rank",status.Get_source())
        for i in range(nrl+2):
            print(u[i,:])
        print()
else:
    comm.Send([w,MPI.DOUBLE],dest=0,tag=rank)

