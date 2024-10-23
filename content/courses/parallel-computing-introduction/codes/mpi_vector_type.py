import sys
import numpy as np
from mpi4py import MPI

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
nprocs = comm.Get_size()

N = nprocs
nr = N+2
nc = N

u = np.arange(1,nr*nc+1).reshape(nr,nc)

w = np.zeros_like(u)

#Cyclic sending
if rank == nprocs-1:
    src=rank-1
    dest=0
elif (rank==0):
    src=nprocs-1
    dest=1
else:
    src=rank-1
    dest=rank+1

#These values pick a total of nc (ncount) items, one item
#(blocklength) taken for each nr (stride) items

#The length of the column is the number of rows
ncount=nr
#The number of items picked from each stride is 1
blocklength=1
#The length of the row is the number of columns
stride=nc

cols = MPI.DOUBLE.Create_vector(ncount, blocklength, stride)
cols.Commit()

if rank==0:
    recv_request=comm.Irecv([np.frombuffer(w.data,np.double,offset=0),1,cols],src)
    send_request=comm.Isend([np.frombuffer(u.data,np.double,offset=0),1,cols],dest)
elif rank==nprocs-1:
    sendcol=nprocs-1
    recv_request=comm.Irecv([np.frombuffer(w.data,np.double,offset=sendcol*np.dtype('double').itemsize),1,cols],src)
    send_request=comm.Isend([np.frombuffer(u.data,np.double,offset=sendcol*np.dtype('double').itemsize),1,cols],dest)
else:
    sendcol=rank
    recv_request=comm.Irecv([np.frombuffer(w.data,np.double,offset=sendcol*np.dtype('double').itemsize),1,cols],src)
    send_request=comm.Isend([np.frombuffer(u.data,np.double,offset=sendcol*np.dtype('double').itemsize),1,cols],dest)

requests=[recv_request,send_request]

MPI.Request.Waitall(requests)

cols.Free()

#Print neatly

#U is the same for each rank in this example

if rank==0:
    print("U")
    for i in range(nr):
        for j in range(nc):
            print("{:12.6f}".format(u[i,j]),end="")
        print()

    print()

comm.Barrier()

print("W for rank", rank)
comm.Barrier()
for i in range(nr):
    for j in range(nc):
        print("{:12.6f}".format(w[i,j]),end="")
    print()

print()


