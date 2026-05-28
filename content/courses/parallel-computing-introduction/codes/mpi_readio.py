import sys
import numpy as np
from mpi4py import MPI

if len(sys.argv)<2:
    print("Usage: filename")
    exit()
else:
    filename=sys.argv[1]

comm=MPI.COMM_WORLD
rank=comm.Get_rank()
nprocs=comm.Get_size()

root=0

#All processes read entire file
amode=MPI.MODE_RDONLY
fh=MPI.File.Open(comm,filename,amode)
fsize=fh.Get_size()
itembytes=MPI.BYTE.Get_size()
if rank==root:
    print("File size is "+str(fsize)+" type size is "+str(itembytes)+" bytes")
nbytes=fsize//itembytes
rbuf=np.empty((nbytes,),dtype='byte')
fh.Read_all([rbuf,MPI.BYTE])
all_chars=[chr(rbuf[i]) for i in range(rbuf.size)]
print("Rank "+str(rank)+" "+''.join(all_chars))
fh.Close()
