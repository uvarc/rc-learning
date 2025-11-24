import sys
import numpy as np
from mpi4py import MPI

if len(sys.argv)<2:
    print("Usage: number of samples")
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

#Arbitrary values.
loc_u=np.ones((nrl,ncl),dtype="int")*(rank+1)

myfile=f'{filename:}{rank:02}'
myfh=open(myfile,'w')
for i in range(nrl):
    print(loc_u[i,:],file=myfh)

locarr=MPI.INT.Create_subarray(gdims,ldims, starts, order=MPI.ORDER_C)
locarr.Commit()

amode=MPI.MODE_CREATE | MPI.MODE_WRONLY
fh=MPI.File.Open(comm,filename,amode)

disp=0
if rank==0:
    fh.Write(gdims)

disp=MPI.INT.Get_size()*gdims.size

fh.Set_view(disp,MPI.INT,locarr,"native",MPI.INFO_NULL)
fh.Write_all(loc_u)

fh.Close()

