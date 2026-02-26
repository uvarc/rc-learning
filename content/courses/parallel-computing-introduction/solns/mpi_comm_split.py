import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

#If not even, then odd
if rank%2==0:
    color=0
else:
    color=1

new_comm=comm.Split(color)

new_comm_size=new_comm.Get_size()
new_comm_rank=new_comm.Get_rank()

message=np.array(['               '],dtype='str')
if new_comm_rank==0:
    if rank%2==0:
        message=np.array(["The secret is 1"],dtype='str')
    else:
        message=np.array(["The secret is 2"],dtype='str')

new_comm.Bcast([message,MPI.CHAR])
print(rank,new_comm_rank,message[0])
