import sys
import numpy as np
from mpi4py import MPI

# Calculate a definite integral using trapezoid rule
# Does not use scipy so we can parallelize it
                                                                                
def f(x):
    return np.sin(x)

def trap(a,b,h,n,f):
    integral=(f(a) + f(b))/2.0
    x=a
    for i in range(n):
        x+=h
        integral+=f(x)
    integral*=h
    return integral

def main():

    comm=MPI.COMM_WORLD
    nprocs=comm.Get_size()
    rank=comm.Get_rank()

    a=np.empty(1,dtype='float')
    b=np.empty(1,dtype='float')
    n=np.empty(1,dtype='int')
    shutdown=None

    if rank==0:
        if len(sys.argv)==4:
            a[0]=float(sys.argv[1])
            b[0]=float(sys.argv[2])
            n[0]=int(float(sys.argv[3]))
            shutdown=False
        else:
            print("Usage: lower bound, upper bound, number of steps")
            shutdown=True
       #Not going to bother with redistributing on this one
        if (not shutdown):
            if n[0]%nprocs!=0:
                print("The number of processes must evenly divide the number of steps ")
                shutdown=True
    
    #Note lower case and return value
    shutdown=comm.bcast(shutdown,root=0)
    if shutdown:
        MPI.Finalize()
        exit()
    
    comm.Bcast([a,MPI.FLOAT],root=0)
    comm.Bcast([b,MPI.FLOAT],root=0)
    comm.Bcast([n,MPI.INT],root=0)

    h=(b-a)/n
    local_n=n[0]//nprocs
    local_a=a[0]+rank*local_n*h
    local_b=b[0]+rank*local_n*h
    
    integral=np.empty(1,dtype='float')
    integral[0]=trap(local_a,local_b,h,local_n,f)

    total=np.empty(1)
    comm.Reduce(integral,total,op=MPI.SUM)
    
    if rank==0:
        print("Result ",total)
    
    MPI.Finalize()
    
if __name__=="__main__":
    main()
