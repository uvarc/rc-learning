from mpi4py import MPI
import numpy as np
import sys

N_per_proc=1000000
num_child_procs=5

comm = MPI.COMM_SELF.Spawn(sys.executable,args=['cpi.py'], maxprocs=num_child_procs)

N = np.array([N_per_proc],dtype='int')
comm.Bcast([N, MPI.INT], root=MPI.ROOT)

pi = np.zeros(1,dtype='float')

comm.Reduce(None, [pi, MPI.DOUBLE], op=MPI.SUM, root=MPI.ROOT)
print(pi)

comm.Disconnect()
