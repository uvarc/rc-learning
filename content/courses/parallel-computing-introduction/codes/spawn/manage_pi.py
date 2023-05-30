from mpi4py import MPI
import numpy as np
import sys

if (len(sys.argv)>1):
    try:
        numPoints_per_proc=int(float((sys.argv[1])))
    except:
        print("Argument must be an integer.")
        exit()
else:
    print("USAGE:python manage_pi.py numPoints")
    exit()

max_procs=5

comm = MPI.COMM_SELF.Spawn(sys.executable,args=['compute_pi.py'], maxprocs=max_procs)

numPoints = np.array([numPoints_per_proc],dtype='int')
comm.Bcast([numPoints, MPI.INT], root=MPI.ROOT)
print(MPI.UNIVERSE_SIZE)

myPi = np.zeros(1,dtype='float')
Pi = np.zeros(1,dtype='float')
comm.Reduce(myPi, Pi, op=MPI.SUM, root=MPI.ROOT)
pi_value=Pi[0]/num_child_procs
print(pi_value)

comm.Disconnect()
