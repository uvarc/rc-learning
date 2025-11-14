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

nrows=2
ncols=3

if nrows*ncols != nprocs:
    print("Number of rows times columns does not equal nprocs")
    sys.exit()

if N%nrows==0 and M%ncols==0:
    nrl = N//nrows
    ncl = M//ncols
else:
    print("Number of ranks should divide the number of rows evenly.")
    sys.exit()

#Set up the topology
ndims=2
dims=[nrows, ncols]
periods=[True, False]
grid_comm=comm.Create_cart(dims, periods)

grid_rank=grid_comm.Get_rank()
grid_coords=grid_comm.coords

direction=0
displ=1
(up,down)=grid_comm.Shift(direction, displ)
direction=1
(left,right)=grid_comm.Shift(direction,displ)

#print("Topo {:d} {:} {:d} {:d} {:d}".format(grid_rank,left,right,up,down))

w = np.zeros((nrl+2, ncl+2), dtype=np.double)

#Arbitrary values.
w[:,:] = np.reshape(np.arange(1,(nrl+2)*(ncl+2)+1),(nrl+2,ncl+2))
w=w*(rank+1)

#Bouncary conditions
topBC=0.
bottomBC=200.
edgeBC=100.
if grid_coords[0]==0:
    w[0,:]=topBC
if grid_coords[0]==nrows-1:
    w[nrl+1,:]=bottomBC
if grid_coords[1]==0:
    w[:,0]=edgeBC
if grid_coords[1]==ncols-1:
    w[:,ncl+1]=edgeBC

# set up MPI type for left column
column_zero=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl,1],[1,0])
column_zero.Commit()

column_one=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl,1],[1,1])
column_one.Commit()

column_end=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl,1],[1,ncl])
column_end.Commit()

column_endbc=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl,1],[1,ncl+1])
column_endbc.Commit()

tag=0

# sending up and receiving down
grid_comm.Sendrecv([w[1,0:ncl+2],MPI.DOUBLE], up, tag, [w[nrl+1,0:ncl+2],MPI.DOUBLE], down, tag)
# sending down and receiving up
grid_comm.Sendrecv([w[nrl,0:ncl+2],MPI.DOUBLE], down, tag, [w[0,0:ncl+2],MPI.DOUBLE], up, tag)

# sending left and right
grid_comm.Sendrecv([w,1,column_one], left, tag, [w,1,column_endbc], right, tag)
grid_comm.Sendrecv([w,1,column_end], right, tag, [w,1,column_zero], left, tag)

#Results
status=MPI.Status()
#Check row exchange
u=np.zeros_like(w)
uwsize=(nrl+2)*(ncl+2)

grid_comm.Barrier()
if rank==0:
    grid_comm.Recv([u,MPI.DOUBLE],source=1,tag=1,status=status)
    print("Ranks 0 and 1 check columns")

    for i in range(nrl+2):
        print(w[i,:],end='')
        print("  |  ",end='')
        print(u[i,:])
    
    print()

if rank==1:
    grid_comm.Send([w,MPI.DOUBLE],dest=0,tag=1)

grid_comm.Barrier()
u[:,:]=0.0
if rank==1:
    grid_comm.Recv([u,MPI.DOUBLE],source=2,tag=2,status=status)
    print("Ranks 1 and 2 check columns")

    for i in range(nrl+2):
        print(w[i,:],end='')
        print("  |  ",end='')
        print(u[i,:])
    
    print()

if rank==2:
    grid_comm.Send([w,MPI.DOUBLE],dest=1,tag=2)

#Checkrows including periodic
grid_comm.Barrier()
u[:,:]=0.0
if rank==0:
    grid_comm.Recv([u,MPI.DOUBLE],source=3,tag=3,status=status)
    print("Ranks 0 and 3 check rows including periodic exchange")

    for i in range(nrl+2):
        print(w[i,:])

    print('-----------------------------------------')

    for i in range(nrl+2):
        print(u[i,:])

if rank==3:
    grid_comm.Send([w,MPI.DOUBLE],dest=0,tag=3)

