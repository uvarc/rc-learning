import numpy as np
import sys
from mpi4py import MPI

filename=sys.argv[1]

comm=MPI.COMM_WORLD
fh=MPI.File.Open(comm,filename,MPI.MODE_RDONLY)


dims=np.empty((2,),dtype='int')
fh.Read(dims)
N=dims[0]; M=dims[1]
print(dims,np.prod(dims))
x=np.empty(N*M+1,dtype='int')
offset=MPI.INT.Get_size()*dims.size
fh.Read(x)

print(x)
sys.exit()

u=np.empty((N,M))

u=np.zeros((N,M))
for j in range(M):
    for i in range(N):
        u[i,j]=x[i+2,j+2]


print(x)
