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

#Hand-distributing numbers
sendcounts=np.array([12,12,11,12,13,9,10,8])
offsets=np.array([0,2,3,1,4,1,1,2])
displs=np.zeros(nprocs,dtype=int)

displs[0]=offsets[0]
for i in range(1,nprocs):
    displs[i]=displs[i-1]+sendcounts[i-1]+offsets[i]

start=np.zeros(nprocs,dtype=int)
end  =np.zeros(nprocs,dtype=int)
# Index into values
start_index=displs[rank]
end_index=displs[rank]+sendcounts[rank]
myvals=values[start_index:end_index]
print(rank,myvals)

# Receive buffer
allvals=np.zeros(101)
comm.Gatherv(myvals,[allvals,sendcounts,displs,MPI.DOUBLE])

if (rank==0):
    print(allvals)
