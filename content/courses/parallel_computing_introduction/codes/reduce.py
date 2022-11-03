import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

num=100
myvals=(rank+1)*np.linspace(1.,num,int(num))

#Add the numbers locally
mysum=myvals.sum()

#Get the grand total
total=np.zeros(1)

comm.Reduce(mysum,total,op=MPI.SUM)

print(rank,mysum,total)
