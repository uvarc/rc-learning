import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

if nprocs !=2:
    print("This program works only for two processes.")
    sys.exit()

baton=np.empty(1)
status=MPI.Status()

if rank==0:
    comm.Recv([baton,MPI.DOUBLE],source=1,status=status)
    baton+=1.
    comm.Send([baton,MPI.DOUBLE],dest=1)
    print("Source",status.Get_source(),"Tag",status.Get_tag(),"Item count",status.Get_count(MPI.DOUBLE),"Error",status.Get_error())
elif rank==1:
    baton[:]=12.
    comm.Send([baton,MPI.DOUBLE],dest=0)
    comm.Recv([baton,MPI.DOUBLE],source=0,status=status)

print(rank,baton)
