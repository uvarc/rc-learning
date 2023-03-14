import numpy as np
import random
import sys
import numpy as np
from mpi4py import MPI

comm=MPI.COMM_WORLD
myrank=comm.Get_rank()
nprocs=comm.Get_size()

if len(sys.argv)>1:
   Nsteps=int(sys.argv[1])
else:
   print(0, 0., 0.)
   sys.exit()

pos=np.array([0,0])

moves=[np.array([0,1]),np.array([0,-1]),np.array([1,0]),np.array([-1,0])]

for n in range(Nsteps):
    move=random.choice(moves)
    pos+=move

distance=np.sqrt(pos[0]**2+pos[1]**2)
total=np.zeros(1)
comm.Reduce(distance,total,op=MPI.SUM)

if myrank==0:
   print(Nsteps, np.sqrt(Nsteps), total/nprocs)

MPI.Finalize()
