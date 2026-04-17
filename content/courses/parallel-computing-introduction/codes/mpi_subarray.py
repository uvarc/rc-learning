import sys
import numpy as np
from mpi4py import MPI

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
nprocs = comm.Get_size()
root = 0

# For simplicity, run with 2 processes
if nprocs != 2:
    print("Please run with exactly 2 processes")
    sys.exit()

nrl=6
ncl=8

# Be sure to match with MPI Type
w = np.zeros((nrl, ncl), dtype='int32')

if rank==root:
    for i in range(nrl):
        for j in range(ncl):
            w[i,j]=i*100+j

# Define the subarrays
sizes = [nrl,ncl]
subsizes=[3,4]

# Set up MPI type for send buffer
starts=[2,3]
sendtype=MPI.INTEGER.Create_subarray(sizes,subsizes,starts)
sendtype.Commit()

# Set up MPI type for receive buffer
starts=[3,2]
recvtype=MPI.INTEGER.Create_subarray(sizes,subsizes,starts)
recvtype.Commit()

# Send subarray from root to rank 1
if rank==root:
    print("Rank ",rank," sending array")
    print(w)
    comm.Send([w,1,sendtype], dest=1, tag=0)
else:
    comm.Recv([w,1,recvtype], source=root, tag=0)
    # Print received values
    print("Rank ",rank," received values")
    print(w)

sendtype.Free()
recvtype.Free()

