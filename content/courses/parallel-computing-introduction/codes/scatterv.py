import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
nprocs=comm.Get_size()
rank=comm.Get_rank()

#For illustration only
if nprocs != 8:
    print("Example requires 8 processes")
    sys.exit()

values=np.linspace(0.,100.,101)
if rank==0:
    print(values)

#Hand-distributing numbers
sendcounts=np.array([12,12,11,12,13,9,10,8])
offsets=np.array([0,2,3,1,4,1,1,2])
displs=np.zeros(nprocs,dtype=int)

displs[0]=offsets[0]
for i in range(1,nprocs):
    displs[i]=displs[i-1]+sendcounts[i-1]+offsets[i]

myvals=np.zeros(sendcounts[rank])

comm.Scatterv([values,sendcounts,displs,MPI.DOUBLE],myvals)

print(rank,myvals)
