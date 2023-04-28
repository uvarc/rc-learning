import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

pong=np.ones(1)*np.pi

if rank==0:
    comm.Send([pong,MPI.DOUBLE],dest=1)
