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

my_val=(rank+1)*np.pi

# Set up the buffer
buf=np.array([my_val])

status=MPI.Status()

amode=MPI.MODE_CREATE | MPI.MODE_WRONLY
fh=MPI.File.Open(comm,filename,amode)

itembytes=MPI.DOUBLE.Get_size()
offset=rank*itembytes
fh.Write_at(offset, [buf, MPI.DOUBLE],status=status)

fh.Close()

if rank==0:
    #First read back with ordinary IO
    with open(filename,'rb') as fp:
        array=np.fromfile(fp,dtype='float')
    array=[str(array[i]) for i in range(array.size)]
    print("Read at root "+"".join(array))
    fp.close()

#All processes read entire file
#Blocking so will wait for root to finish above read
amode=MPI.MODE_RDONLY
fh=MPI.File.Open(comm,filename,amode)
fsize=fh.Get_size()

nvals=fsize//itembytes
rbuf=np.empty((nvals,))
fh.Read_all([rbuf,MPI.DOUBLE])
all_chars=[str(rbuf[i]) for i in range(rbuf.size)]
print("Entire file at rank "+str(rank)+" "+' '.join(all_chars))
fh.Close()

#Read back the MPI file portion for each rank
amode=MPI.MODE_RDONLY
fh=MPI.File.Open(comm,filename,amode)

my_val=0
offset=rank*itembytes
fh.Read_at(offset, [buf, MPI.DOUBLE])
fh.Close()
my_val=buf[0]
print(str(rank)+' '+str(my_val))
