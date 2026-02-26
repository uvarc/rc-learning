import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

world_group=comm.group

evens=[]

#If not even, then odd
for myrank in range(nprocs):
    if myrank%2==0:
        evens.append(myrank)

if rank in evens:
    new_group=world_group.Incl(evens)
else:
    new_group=world_group.Excl(evens)

new_comm=comm.Create(new_group)

new_comm_size=new_comm.Get_size()
new_comm_rank=new_comm.Get_rank()

message=np.array(['               '],dtype='str')
if new_comm_rank==0:
    if rank in evens:
        message=np.array(["The secret is 1"],dtype='str')
    else:
        message=np.array(["The secret is 2"],dtype='str')

new_comm.Bcast([message,MPI.CHAR])
print(rank,new_comm_rank,message[0])
