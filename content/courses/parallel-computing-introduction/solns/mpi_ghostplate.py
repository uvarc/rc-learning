import sys
import time
import numpy as np
from mpi4py import MPI

def set_bcs(u,nrl,ncl,nrows,ncols,coords):
    # Set physical boundary values and compute mean boundary value.
    # Due to Python pass-by-assignment, mutable parameters will be
    #changed outside (so this is a subroutine)

    topBC=0.
    bottomBC=100.
    leftBC = 100.
    rightBC = 100.

    if coords[0]==0:
        u[0:nghosts,:]=topBC
    if coords[0]==nrows-1:
        u[nrl+nghosts:,:]=bottomBC
    if coords[1]==0:
        u[:,0:nghosts]=leftBC
    if coords[1]==ncols-1:
        u[:,ncl+nghosts:]=rightBC

# check number of parameters and read in parameters
#Better to use argparse but this is closer to compiled language code and simpler
#if one hasn't used argparse.

argv=sys.argv

if  len(argv) > 2:
   try:
      epsilon=float(argv[1])
      filename=argv[2]
   except:
      "Illegal input"
      sys.exit(1)
   if len(argv) == 3:
       N=500
       M=500
   if len(argv) == 4:
       try:
          N=int(argv[3])
          M=N
       except:
           "Cannot convert grid dimension to integer"
           sys.exit(2)
   if len(argv) == 5:
       try:
           N=int(argv[3])
           M=int(argv[4])
       except:
           "Cannot convert grid dimension to integer"
           sys.exit(2)
else:
   print('USAGE: epsilon output-file <N> <M>')
   sys.exit(1)

#Set up MPI
comm = MPI.COMM_WORLD
rank = comm.Get_rank()
nprocs = comm.Get_size()

root=0
tag=0

#Set number of rows and columns
#For simplicity, require a perfect square number of processes
nrows=np.sqrt(nprocs)
if nrows != int(nrows):
    print("This code requires a perfect square number of processes")
    sys.exit()
else:
    nrows=int(nrows)
    ncols=nrows

#Strong scaling
if M%nrows!=0:
    print("Number of rows must be evenly divisible by number of processes")
    sys.exit(0)
else:
    nrl = N//nrows
    ncl = N//ncols

#Set max iterations. 10 million should do it.
max_iterations=10000000

#Set up the topology
ndims=2
dims=[nrows, ncols]
periods=[False, False]
grid_comm=comm.Create_cart(dims, periods)

grid_rank=grid_comm.Get_rank()
grid_coords=grid_comm.coords

direction=0
displ=1
(up,down)=grid_comm.Shift(direction, displ)
direction=1
(left,right)=grid_comm.Shift(direction,displ)

#Assume same number of ghost zones for rows and columns
nghosts=2
w_halo=2*nghosts
nrl_total=nrl+w_halo
ncl_total=ncl+w_halo

#Solution arrays
u = np.zeros((nrl_total, ncl_total), dtype=np.double)
w = np.zeros((nrl_total, ncl_total), dtype=np.double)

set_bcs(u,nrl,ncl,nrows,ncols,grid_coords)

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

# Compute steady-state solution
iteration=0
diff_interval=1

start_time=MPI.Wtime()
my_diff = np.zeros(1)
global_diff=np.zeros(1)

nrl_gz=nrl+nghosts
ncl_gz=ncl+nghosts

if rank==root:
    print(f'Running until the difference is < {epsilon:}, global size {N:}x{M:}')

while iteration<=max_iterations:

    rcv_down =comm.Irecv([u, 1, rbuf_down],  down)
    rcv_up   =comm.Irecv([u, 1, rbuf_up],    up)
    rcv_left =comm.Irecv([u, 1, rbuf_left],  left)
    rcv_right=comm.Irecv([u, 1, rbuf_right], right)

    snd_up   =comm.Isend([u, 1, sbuf_up],    up)
    snd_down =comm.Isend([u, 1, sbuf_down],  down)
    snd_right=comm.Isend([u, 1, sbuf_right], right)
    snd_left =comm.Isend([u, 1, sbuf_left],  left)

    requests=[rcv_down,rcv_up,snd_up,snd_down,
              rcv_left,rcv_right,snd_right,snd_left]

    MPI.Request.Waitall(requests)

    w[nghosts:nrl_gz,nghosts:ncl_gz]=0.25*(u[nghosts-1:nrl_gz-1,nghosts:ncl_gz]+
                                           u[nghosts+1:nrl_gz+1,nghosts:ncl_gz]+
                                           u[nghosts:nrl_gz,nghosts-1:ncl_gz-1]+
                                           u[nghosts:nrl_gz,nghosts+1:ncl_gz+1])

    if iteration%diff_interval==0:
        my_diff[0]=np.max(np.abs(w[nghosts:nrl_gz,nghosts:ncl_gz]-
                                 u[nghosts:ncl_gz,nghosts:ncl_gz]))
        comm.Allreduce(my_diff,global_diff,op=MPI.MAX)

        if global_diff[0]<=epsilon:
            break

    #Update u
    u[nghosts:nrl_gz,nghosts:ncl_gz]=w[nghosts:nrl_gz,nghosts:ncl_gz]

    #Reapply physical boundary conditions
    set_bcs(u,nrl,ncl,nrows,ncols,grid_coords)

    iteration+=1

#This is what the weird "else" clause in Python for/while loops is used to do.
#Our stopping criterion is now on exceeding max_iterations so don't need
# to break, but we need to warn that it happened.
else:
     if grid_rank==0:
         print("Warning: maximum iterations exceeded")

total_time=MPI.Wtime()-start_time

if grid_rank==0:
    print(f'completed in {iteration:} iterations with time {total_time:.2f}')

# Write solution to output file
filename = filename + str(grid_coords[0]) + str(grid_coords[1])
fout = open (filename,'w')
for i in range(1,nrl+1):
    line=" ".join(map(str,list(u[i,1:ncl+1])))
    row=line+"\n"
    fout.write(row)

# All done!
#print(f"wrote output file {filename:}")
