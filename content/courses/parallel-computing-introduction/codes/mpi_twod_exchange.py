import sys
import numpy as np
from mpi4py import MPI

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
nprocs = comm.Get_size()

N = 500
M = 500

#Divide up the processes.  Either we require a perfect square, or we
#must specify how to distribute by row/column.  In a realistic program,
#the process distribution (either the total, for a perfect square, or
#the rows/columns) would be read in and we would need to check that the number
#of processes requested is consistent with the decomposition.

nproc_rows=5
nproc_cols=5

if nproc_rows*nproc_cols != nprocs:
    print("Number of rows times columns does not equal nprocs")
    sys.exit()

#Strong scaling
if N%nprocs==0 and M%nprocs==0:
    nrl = N//nproc_rows
    ncl = M//nproc_cols
else:
    print("Number of ranks should divide the number of rows evenly.")
    sys.exit()

#Weak scaling
#nrl = N
#ncl = M

w = np.zeros((nrl+2, ncl+2), dtype=np.double)

#Set up the topology assuming processes numbered left to right by row

my_row=rank%nproc_rows
my_col=rank%nproc_cols

#Set up boundary conditions
if my_row == 0:
    w[0,:] = 0.        # up

if my_row == nproc_rows-1 :
    w[nrl+1,:] = 100.   # bottom

if my_col == 0:  
    w[:,0] = 100.      # left

if my_col == nproc_cols-1:
    w[:,ncl+1] = 100.   # right

#Arbitarary value for interior that may speed up convergence somewhat.
#Be sure not to overwrite boundaries.
w[1:nrl+1,1:ncl+1] = 50.

# setting up the up and down rank for each process
if my_row == 0 :
    up = MPI.PROC_NULL
else :
    up = rank - ncols

if my_row == nprocs - 1 :
    down = MPI.PROC_NULL
else :
    down = rank + ncols

if my_col == 0 :
    left = MPI.PROC_NULL
else:
    left = rank-1

if my_col == ncols-1:
    right = MPI.PROC_NULL
else:
    right = rank+1

# set up MPI vector type for column
column=MPI.DOUBLE.Create_vector(

tag=0

# sending up and receiving down
comm.Sendrecv([w[1,1:ncl+1],MPI.DOUBLE], up, tag, [w[nrl+1,1:ncl+1],MPI.DOUBLE], down, tag)
# sending down and receiving up
comm.Sendrecv([w[nrl,1:ncl+1],MPI.DOUBLE], down, tag, [w[0,1:ncl+1],MPI.DOUBLE], up, tag)

# sending right and left.

# Spot-check result
for n in range(nprocs):
    if n==rank:
        print(n,w[0,ncl//2],w[nrl+1,ncl//2])
