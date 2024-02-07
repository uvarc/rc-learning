import sys
import numpy as np
from mpi4py import MPI

def do_work():
    nsteps=rng.integers(low=10000, high=30000)
    result=0
    for i in range(nsteps):
        result+=i
    return np.array([result],dtype='float')

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

status=MPI.Status()

# The topic of handling random-number generation in parallel programs is 
# an issue in applied mathematics, and beyond our scope here. This should 
# generally space out the seeds a bit.
# rng is global
rng = np.random.default_rng()

done=np.array([0])
results=np.zeros(nprocs)

if rank==0:
    result=np.empty(1)
    for i in range(1,nprocs):
        comm.Recv(....
        sender=
        results[sender]=result
        done[:]=1
        comm.Send([done,MPI.INT],dest=sender)
else: 
    for n in range(1,nprocs):
        if (rank==n):
            result=do_work()
            comm.Send(
            comm.Recv(

total=np.sum(results)
if rank==0:
    print(f"The final result is {total:e}")

#Disconnect all processes from communicator
comm.Disconnect()
