import sys
import time
import numpy as np
from mpi4py import MPI

def set_bcs(u,nrl,ncl,rank):
    # Set physical boundary values and compute mean boundary value. 
    # Due to Python pass-by-assignment, mutable parameters will be
    #changed outside (so this is a subroutine)

    topBC=0.
    bottomBC=100.
    edgeBC = 100.

    bc1=topBC
    bc2=bottomBC
    bc3=edgeBC
    bc4=edgeBC

    if rank==0:
        u[0,:] = topBC
    if rank==nprocs-1:
        u[nrl+1,:]=bottomBC

    u[:,0]=edgeBC
    u[:,ncl+1]=edgeBC

    return bc1,bc2,bc3,bc4

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

#Set nprocs of local arrays

#Strong scaling
if M%nprocs!=0:
    print("Number of rows must be evenly divisible by number of processes")
    sys.exit(0)
else:
    nrl = N//nprocs

#Weak scaling
#nrl=N

#Both
ncl=M

u=np.zeros((nrl+2,ncl+2))
w=np.zeros((nrl+2,ncl+2))

bc1,bc2,bc3,bc4=set_bcs(u,nrl,ncl,rank)

#Set up the up and down rank for each process
if rank == 0 :
    up = MPI.PROC_NULL
else :
    up = rank - 1

if rank == nprocs - 1 :
    down = MPI.PROC_NULL
else :
    down = rank + 1

# Compute steady-state solution
iterations=0
diff_interval=1

start_time=MPI.Wtime()
my_diff = np.zeros(1)
global_diff=np.zeros(1)

if rank==root:
    print(f'Running until the difference is < {epsilon:}, global size {N:}x{M:}')

while iterations<=max_iterations:

   # Send up and receive down
    comm.Sendrecv([u[1,1:ncl+1], MPI.DOUBLE], up, tag, [u[nrl+1,1:ncl+1], MPI.DOUBLE], down, tag)
    # Send down and receive up
    comm.Sendrecv([u[nrl, 1:ncl+1], MPI.DOUBLE], down, tag, [u[0,1:ncl+1], MPI.DOUBLE], up, tag)

    w[1:-1,1:-1]=0.25*(u[:-2,1:-1]+u[2:,1:-1]+u[1:-1,:-2]+u[1:-1,2:])

    if iterations%diff_interval==0:
        my_diff[0]=np.max(np.abs(w[1:-1,1:-1]-u[1:-1,1:-1]))
        comm.Allreduce(my_diff,global_diff,op=MPI.MAX)

        if global_diff[0]<=epsilon:
            break

    u[:,:]=w[:,:]
    #Reapply physical boundary conditions
    set_bcs(u,nrl,ncl,rank)

    iterations+=1
#This is what the weird "else" clause in Python for/while loops is used to do.
#Our stopping criterion is now on exceeding max_iterations so don't need
# to break, but we need to warn that it happened.
else:
     if rank==0:
         print("Warning: maximum iterations exceeded")

total_time=MPI.Wtime()-start_time

if rank==0:
    print(f'completed in {iterations:} iterations with time {total_time:.2f}')

# Write solution to output file
filename = filename + str(rank)
fout = open (filename,'w')
for i in range(1,nrl+1):
    line=" ".join(map(str,list(u[i,1:ncl+1])))
    row=line+"\n"
    fout.write(row)

# All done!
#print(f"wrote output file {filename:}")
