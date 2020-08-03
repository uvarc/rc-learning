""" 
This is a program with mpi to solve a weakly scaled problem (i.e. Steady state 
Heat Distribution problem with boundary condition) for square grid (for each 
process) 
"""
import sys
import numpy as np
from mpi4py import MPI
# check number of parameters and read in epsilon

argv = sys.argv
if  len(argv) == 3:
    try:
        epsilon = float( argv[1] )
        filename=argv[2]
        N = 500
    except:
        "Illegal input"
        sys.exit(1)
elif len(argv) == 4 :
    try:
        epsilon = float( argv[1] )
        filename=argv[2]
        N = int( argv[3] )
    except:
        "Illegal input"
        sys.exit(1)
else:
    print'USAGE: <epsilon> <output-file> <Square-Grid-Size(optional)>'
    sys.exit(1)

# Initializing MPI
comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()
nroot = 0
intrvlGlobDiffCheck = 10 # interval for calculating maximum global diff

# Benchmarking
elapsed_time = 0.
comm.Barrier()
elapsed_time = -MPI.Wtime()
localDiff = np.array( [epsilon] )
globalDiff = np.array( [epsilon] )
if rank == nroot :
    print '############ Steady State Heat Distribution Problem #############'
    print 'running until the difference is < ', epsilon
    

#The local array is square
nr = N 
nc = N 

u = np.zeros(  ( nr+2, nc+2 ), dtype=np.double  )
w = np.zeros(  ( nr+2, nc+2 ), dtype=np.double   )

# Set boundary values and compute mean boundary value. 
#This has the ice bath on the top edge, not the bottom edge.
if rank == 0 : 
    u[ 0 , : ] = 0.        # Up
u[ : , 0 ] = 100.      # left
u[ : , nc+1 ] = 100.   # right
if rank == size-1 :
    u[ nr+1 , : ] = 100.   # bottom


#Initialize interior values to the boundary mean.  This value really
#doesn't matter much, it just affects the convergence rate somewhat.
mean = sum( u[:,0] ) + sum( u[:,nr+1] ) + sum( u[0,:] ) + sum( u[nr+1,:] )
mean = mean / ( 4.0 * (N+2) )

u[ 1:nr , 1:nc ] = mean

# setting up the up and down rank for each process
if rank == 0 :
    up = MPI.PROC_NULL
else :
    up = rank - 1
    
if rank == size - 1 :
    down = MPI.PROC_NULL
else :
    down = rank + 1

# Compute steady-state solution
iterations = 0
tag=0

while ( globalDiff[0] >= epsilon ) :
    # sending up and receiving down
    comm.Sendrecv(  [  u[1, 1:nc+1], MPI.DOUBLE  ], up, tag, [  u[nr+1, 1:nc+1], MPI.DOUBLE  ], down , tag )
    # sending down and receiving up
    comm.Sendrecv(  [  u[ nr , 1:nc+1 ] , MPI.DOUBLE ] , down, tag, [  u[ 0, 1:nc+1 ], MPI.DOUBLE  ], up , tag )
    # calculating average
    w[ 1:-1, 1:-1 ] = 0.25 * ( u[:-2,1:-1] +u[2:,1:-1] + u[1:-1,:-2] + u[1:-1,2:] )
    
    localDiff[0] = np.max(  np.abs( w[1:-1,1:-1] - u[1:-1,1:-1] )  )
    
    if iterations % intrvlGlobDiffCheck == 0 :
        comm.Allreduce( localDiff, globalDiff, op = MPI.MAX  )
        #if rank == nroot :
        #    print "Running with iterations :", iterations, 'and global diff : ', globalDiff[0]
    
    if globalDiff[0] >= epsilon :
        u[ 1:N+1 , 1:N+1 ]  =  w[ 1:N+1 , 1:N+1 ].copy() 
    iterations += 1
if rank == nroot :        
    print 'completed; iterations = ', iterations

# Write solution to output file
filename = filename + str( rank ) + ".txt"
fout = open (filename,'w')
if rank == nroot :
    print  >> fout, N, N, size
for i in range( 1 , N+1 ) :
    line = " ".join(   map(  str , list( u[i, 1:N+1] )  )   )
    row = line + "\n"
    fout.write( row )

# All done!
print  "Process ", rank, " : wrote output file ", filename
# Benchmarking
elapsed_time += MPI.Wtime()

if rank == nroot :
    print 'Elapsed Time : ', elapsed_time, ' sec'
