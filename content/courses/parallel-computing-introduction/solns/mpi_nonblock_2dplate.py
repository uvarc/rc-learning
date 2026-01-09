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
        u[0,:]=topBC
    if coords[0]==nrows-1:
        u[nrl+1,:]=bottomBC
    if coords[1]==0:
        u[:,0]=leftBC
    if coords[1]==ncols-1:
        u[:,ncl+1]=rightBC

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


# Initializing MPI
comm = MPI.COMM_WORLD
rank = comm.Get_rank()
nprocs = comm.Get_size()
root = 0
tag = 0

#Set max iterations. 10 million should do it.
max_iterations=10000000

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

#Set up topology
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

u=np.zeros((nrl+2,ncl+2))
w=np.zeros((nrl+2,ncl+2))

u=np.zeros((nrl+2,ncl+2))
w=np.zeros((nrl+2,ncl+2))

set_bcs(u,nrl,ncl,nrows,ncols,grid_coords)

#Set up types
column_zero=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl,1],[1,0])
column_zero.Commit()

column_one=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl,1],[1,1])
column_one.Commit()

column_end=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl,1],[1,ncl])
column_end.Commit()

column_endbc=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl,1],[1,ncl+1])
column_endbc.Commit()


# Compute steady-state solution
iterations=0
diff_interval=1

start_time=MPI.Wtime()
my_diff = np.zeros(1)
global_diff=np.zeros(1)

if rank==root:
    print(f'Running until the difference is < {epsilon:}, global size {N:}x{M:}')

while iterations<=max_iterations:

    rcv_down =comm.Irecv([w[nrl+1,1:ncl+1],MPI.DOUBLE],down)
    rcv_up   =comm.Irecv([w[0,1:ncl+1],MPI.DOUBLE],up)
    rcv_left =comm.Irecv([u,1,column_zero], left)
    rcv_right=comm.Irecv([u,1,column_endbc], right)

    send_up   =comm.Isend([w[1,1:ncl+1],MPI.DOUBLE], up)
    send_down =comm.Isend([w[nrl,1:ncl+1],MPI.DOUBLE], down)
    send_right=comm.Isend([u,1,column_end], right)
    send_left =comm.Isend([u,1,column_one], left)

    requests=[rcv_down,rcv_up,send_up,send_down,
              rcv_left,rcv_right,send_right,send_left]

    MPI.Request.Waitall(requests)

    w[1:-1,1:-1]=0.25*(u[:-2,1:-1]+u[2:,1:-1]+u[1:-1,:-2]+u[1:-1,2:])

    #set halo values
    w[0,:]=u[0,:]
    w[nrl+1,:]=u[nrl+1,:]
    w[:,0]=u[:,0]
    w[:,ncl+1]=u[:,ncl+1]


    if iterations%diff_interval==0:
        my_diff[0]=np.max(np.abs(w[1:-1,1:-1]-u[1:-1,1:-1]))
        comm.Allreduce(my_diff,global_diff,op=MPI.MAX)

        if global_diff[0]<=epsilon:
            break

    u[:,:]=w[:,:]
    #Reapply physical boundary conditions
    set_bcs(u,nrl,ncl,nrows,ncols,grid_coords)

    iterations+=1
#This is what the weird "else" clause in Python for/while loops is used to do.
#Our stopping criterion is now on exceeding max_iterations so don't need
# to break, but we need to warn that it happened.
else:
     if grid_rank==0:
         print("Warning: maximum iterations exceeded")

total_time=MPI.Wtime()-start_time

if grid_rank==0:
    print(f'completed in {iterations:} iterations with time {total_time:.2f}')

# Write solution to output file
filename = filename + str(grid_coords[0]) + str(grid_coords[1])
fout = open (filename,'w')
for i in range(1,nrl+1):
    line=" ".join(map(str,list(u[i,1:ncl+1])))
    row=line+"\n"
    fout.write(row)

# All done!
#print(f"wrote output file {filename:}")
