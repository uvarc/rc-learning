import sys
import numpy as np
from mpi4py import MPI

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
nprocs = comm.Get_size()

#This example exchanges data among four rectangular domains with halos.
#Most real codes use squares, but we want to illustrate how to use different
#dimensions.

#Divide up the processes.  Either we require a perfect square, or we
#must specify how to distribute by row/column.  In a realistic program,
#the process distribution (either the total, for a perfect square, or
#the rows/columns) would be read in and we would need to check that the number
#of processes requested is consistent with the decomposition.

N = 8
M = 12

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
w[:,:] = np.reshape(np.arange(1,(nrl+2)*(ncl+2)+1),(nrl+2,ncl+2))
w=w*(rank+1)

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

#Boundary conditions
topBC=0.0
bottomBC=200.00
rightBC=100.0
leftBC=100.0
if right == MPI.PROC_NULL:
    w[:,ncl+1]=rightBC
if left == MPI.PROC_NULL:
    w[:,0]=leftBC
if up == MPI.PROC_NULL:
    w[0,:]=topBC
if down == MPI.PROC_NULL:
    w[nrl+1,:]=bottomBC

# set up MPI type for left column
column_zero=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl,1],[1,0])
column_zero.Commit()

column_one=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl,1],[1,1])
column_one.Commit()

column_end=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl,1],[1,ncl])
column_end.Commit()

column_endbc=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl,1],[1,ncl+1])
column_endbc.Commit()


#This forces the output to show one one rank at a time. It is not efficient.
status=MPI.Status()
print("topology", rank, up, down, left, right)
if rank==0:
    u=np.zeros_like(w)
    print("Initial for rank 0")
    print(w)
    for i in range(1,nprocs):
        comm.Recv([u,MPI.DOUBLE],source=MPI.ANY_SOURCE,tag=MPI.ANY_TAG,status=status)
        print("Initial for rank",status.Get_source())
        print(u)
else:
    comm.Send([w,MPI.DOUBLE],dest=0,tag=rank)

comm.Barrier()

tag=0
# sending up and receiving down
comm.Sendrecv([w[1,0:ncl+2],MPI.DOUBLE], up, tag, [w[nrl+1,0:ncl+2],MPI.DOUBLE], down, tag)
# sending down and receiving up
comm.Sendrecv([w[nrl,0:ncl+2],MPI.DOUBLE], down, tag, [w[0,0:ncl+2],MPI.DOUBLE], up, tag)

# sending left and right
comm.Sendrecv([w,1,column_one], left, tag, [w,1,column_endbc], right, tag)
comm.Sendrecv([w,1,column_end], right, tag, [w,1,column_zero], left, tag)

# check result
if rank==0:
    u=np.zeros_like(w)
    print("Final for rank 0")
    print(w)
    for i in range(1,nprocs):
        comm.Recv([u,MPI.DOUBLE],source=MPI.ANY_SOURCE,tag=MPI.ANY_TAG,status=status)
        print("Final for rank",status.Get_source())
        print(u)
else:
    comm.Send([w,MPI.DOUBLE],dest=0,tag=rank)

