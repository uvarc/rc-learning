import sys
import numpy as np
from mpi4py import MPI

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
nprocs = comm.Get_size()

N = 8
M = 12

#This example exchanges data among four rectangular domains with halos.
#Most real codes use squares, but we want to illustrate how to use different
#dimensions.

#Divide up the processes.  Either we require a perfect square, or we
#must specify how to distribute by row/column.  In a realistic program,
#the process distribution (either the total, for a perfect square, or
#the rows/columns) would be read in and we would need to check that the number
#of processes requested is consistent with the decomposition.

nproc_rows=2
nproc_cols=3

if nproc_rows*nproc_cols != nprocs:
    print("Number of rows times columns does not equal nprocs")
    sys.exit()

if N%nproc_rows==0 and M%nproc_cols==0:
    nrl = N//nproc_rows
    ncl = M//nproc_cols
else:
    print("Number of ranks should divide the number of rows evenly.")
    sys.exit()

w = np.zeros((nrl+2, ncl+2), dtype=np.double)

#Set up the topology assuming processes numbered left to right by row
my_row=rank//nproc_cols
my_col=rank%nproc_cols

#Arbitrary values.
w[:,:] = 50.*np.random.rand(1)

# setting up the up and down rank for each process
if my_row == 0 :
    up = MPI.PROC_NULL
else :
    up = rank - nproc_cols

if my_row == nproc_rows-1 :
    down = MPI.PROC_NULL
else :
    down = rank + nproc_cols

if my_col == 0 :
    left = MPI.PROC_NULL
else:
    left = rank-1

if my_col == nproc_cols-1:
    right = MPI.PROC_NULL
else:
    right = rank+1
    

# set up MPI type for left column
column_zero=MPI.DOUBLE.Create_vector(nrl+2,1,ncl+2)
column_zero.Commit()

column_one=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl+2,1],[0,1])
column_one.Commit()

column_end=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl+2,1],[0,ncl])
column_end.Commit()

column_endbc=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl+2,1],[0,ncl+1])
column_endbc.Commit()

tag=0
print('Before')
for n in range(nprocs):
   if n==rank:
       print(n,w)

# sending up and receiving down
comm.Sendrecv([w[1,1:ncl+1],MPI.DOUBLE], up, tag, [w[nrl+1,1:ncl+1],MPI.DOUBLE], down, tag)
# sending down and receiving up
comm.Sendrecv([w[nrl,1:ncl+1],MPI.DOUBLE], down, tag, [w[0,1:ncl+1],MPI.DOUBLE], up, tag)

# sending right and left.
comm.Sendrecv([w,1,column_endbc], right, tag, [w,1,column_one], left, tag)
comm.Sendrecv([w,1,column_zero], left, tag, [w,1,column_end], right, tag)

# Spot-check result
print('After')
for n in range(nprocs):
    if n==rank:
    #    print(n,w[0,ncl//2],w[nrl+1,ncl//2])
        print(n,w)
