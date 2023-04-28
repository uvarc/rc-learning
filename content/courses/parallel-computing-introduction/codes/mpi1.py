from mpi4py import MPI
import sys

myrank = MPI.COMM_WORLD.Get_rank()
nprocs = MPI.COMM_WORLD.Get_size()

if myrank == 0:
    sys.stdout.write("Running on %d processes\n" %(nprocs))

sys.stdout.write("Greetings from process %d\n"%(myrank))

