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

#Assume same number of ghost zones for rows and columns
nghosts=2
w_halo=2*nghosts
nrl_total=nrl+w_halo
ncl_total=ncl+w_halo

w = np.zeros((nrl_total, ncl_total), dtype=np.double)

#Arbitrary values.
w[:,:]=np.reshape(np.arange(1,(nrl_total)*(ncl_total)+1),(nrl_total,ncl_total))
w=w*(grid_rank+1)

#Boundary conditions
#Make it easier to distinguish one side from another for checking
topBC=100
bottomBC=400.
edgeBC=350.
if grid_coords[0]==0:
    w[0:nghosts,:]=topBC
if grid_coords[0]==nrows-1:
    w[nrl+nghosts:,:]=bottomBC
if grid_coords[1]==0:
    w[:,0:nghosts]=edgeBC
if grid_coords[1]==ncols-1:
    w[:,ncl+nghosts:]=edgeBC

# set up MPI types for buffers

sizes = [nrl_total,ncl_total]

# Rows
subsizes  = [nghosts,ncl_total]

starts = [nghosts,0]
sbuf_up=MPI.DOUBLE.Create_subarray(sizes, subsizes, starts)
sbuf_up.Commit()

starts = [nrl+nghosts,0]
rbuf_down=MPI.DOUBLE.Create_subarray(sizes, subsizes, starts)
rbuf_down.Commit()

starts = [nrl,0]
sbuf_down=MPI.DOUBLE.Create_subarray(sizes, subsizes, starts)
sbuf_down.Commit()

starts = [0,0]
rbuf_up=MPI.DOUBLE.Create_subarray(sizes, subsizes, starts)
rbuf_up.Commit()

# Columns
subsizes  = [nrl,nghosts]

starts = [nghosts,ncl]
sbuf_right=MPI.DOUBLE.Create_subarray(sizes, subsizes, starts)
sbuf_right.Commit()

starts = [nghosts,0]
rbuf_left=MPI.DOUBLE.Create_subarray(sizes, subsizes, starts)
rbuf_left.Commit()

starts = [nghosts,nghosts]
sbuf_left=MPI.DOUBLE.Create_subarray(sizes, subsizes, starts)
sbuf_left.Commit()

starts = [nghosts,ncl+nghosts]
rbuf_right=MPI.DOUBLE.Create_subarray(sizes, subsizes, starts)
rbuf_right.Commit()

tag=0

# sending up and receiving down
grid_comm.Sendrecv([w,1,sbuf_up], up, tag, [w,1,rbuf_down], down, tag)
# sending down and receiving up
grid_comm.Sendrecv([w,1,sbuf_down], down, tag, [w,1,rbuf_up], up, tag)

# sending left and right
grid_comm.Sendrecv([w,1,sbuf_left], left, tag, [w,1,rbuf_right], right, tag)
grid_comm.Sendrecv([w,1,sbuf_right], right, tag, [w,1,rbuf_left], left, tag)

#Results
status=MPI.Status()
#Check row exchange
u=np.zeros_like(w)
uwsize=(nrl+2)*(ncl+2)

grid_comm.Barrier()
if grid_rank==0:
    grid_comm.Recv([u,MPI.DOUBLE],source=1,tag=1,status=status)
    print("Ranks 0 and 1 check columns")

    for i in range(nrl_total):
        print(w[i,:],end='')
        print("  |  ",end='')
        print(u[i,:])
    
    print()

if grid_rank==1:
    grid_comm.Send([w,MPI.DOUBLE],dest=0,tag=1)

grid_comm.Barrier()
u[:,:]=0.0
if grid_rank==1:
    grid_comm.Recv([u,MPI.DOUBLE],source=2,tag=2,status=status)
    print("Ranks 1 and 2 check columns")

    for i in range(nrl_total):
        print(w[i,:],end='')
        print("  |  ",end='')
        print(u[i,:])
    
    print()

if grid_rank==2:
    grid_comm.Send([w,MPI.DOUBLE],dest=1,tag=2)

#Check rows including periodic
grid_comm.Barrier()
u[:,:]=0.0
if grid_rank==0:
    grid_comm.Recv([u,MPI.DOUBLE],source=3,tag=3,status=status)
    print("Ranks 0 and 3 check rows including periodic exchange")

    for i in range(nrl_total):
        print(w[i,:])

    print('-----------------------------------------')

    for i in range(nrl_total):
        print(u[i,:])

if grid_rank==3:
    grid_comm.Send([w,MPI.DOUBLE],dest=0,tag=3)

