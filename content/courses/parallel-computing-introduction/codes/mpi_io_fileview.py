import sys
import numpy as np
from mpi4py import MPI

if len(sys.argv)<2:
    print("Usage: filename <opt> nrows <opt> ncols")
    exit()
else:
    filename=sys.argv[1]

if len(sys.argv)==2:
    nrows=4
    ncols=4
elif len(sys.argv)==3:
    nrows=int(float(sys.argv[2]))
    ncols=nrows
elif len(sys.argv)==4:
    nrows=int(float(sys.argv[2]))
    ncols=int(float(sys.argv[3]))

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
nprocs = comm.Get_size()

if nrows*ncols != nprocs:
    print("Number of rows times columns does not equal nprocs")
    sys.exit()

# Hard-code local array size so we can see what we're doing
nrl = 4
ncl = 4

N=nrl*nrows
M=nrl*ncols

#Set up the topology
lrow=rank//ncols
lcol=rank%ncols

ndims=2
gdims=np.array([N,M],dtype='int')
ldims=np.array([nrl,ncl],dtype='int')
starts=np.array([ncl*lrow,nrl*lcol],dtype='int')
print(rank,starts)

#Generate arbitrary values.
loc_u=np.ones((nrl,ncl),dtype="int")*(rank+1)

#Write each segment to conventional file (the "old way")
myfile=f'{filename:}{rank:02}'
myfh=open(myfile,'w')
for i in range(nrl):
    print(loc_u[i,:],file=myfh)
myfh.close()

locarr=MPI.INT.Create_subarray(gdims,ldims, starts, order=MPI.ORDER_C)
locarr.Commit()

amode=MPI.MODE_CREATE | MPI.MODE_WRONLY
fh=MPI.File.Open(comm,filename,amode)

#No header
disp=0
fh.Set_view(disp,MPI.INT,locarr,"native",MPI.INFO_NULL)
fh.Write_all(loc_u)

fh.Close()

amode=MPI.MODE_RDONLY
fh=MPI.File.Open(comm,filename,amode)

buf=np.zeros_like(loc_u,dtype='int')
fh.Set_view(disp,MPI.INT,locarr,"native",MPI.INFO_NULL)
fh.Read_all([buf, MPI.INT])
fh.Close()

#Usual trick to show one rank at a time
status=MPI.Status()
if rank==0:
    u=np.zeros_like(buf)
    print("Read for rank 0")
    print(buf)
    for i in range(1,nprocs):
        comm.Recv([u,MPI.INT],source=MPI.ANY_SOURCE,tag=MPI.ANY_TAG,status=status)
        print("Read for rank ",status.Get_source())
        print(u)
else:
    comm.Send([buf,MPI.INT],dest=0,tag=rank)

comm.Barrier()

