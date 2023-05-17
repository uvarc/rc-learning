"""
 This program estimates the value of PI by running a Monte Carlo simulation.
 NOTE:  This is not how one would normally want to calculate PI, but serves
 to illustrate the principle.
"""

import sys
import os
import math
import numpy as np
from mpi4py import MPI

def throw(numPoints):
    """Throw a series of imaginary darts at an imaginary dartboard of unit 
        radius and count how many land inside the circle."""

    x=np.random.random(numPoints)
    y=np.random.random(numPoints)

    inside=(x**2+y**2)<=1.0
    numInside=sum(inside)
    return numInside

def main():

    if (len(sys.argv)>1):
        try:
            numPoints=int(float((sys.argv[1])))
        except:
            print("Argument must be an integer.")
    else:
        print("USAGE:python MonteCarlo.py numPoints")
        exit()

    myrank = MPI.COMM_WORLD.Get_rank()
    nprocs = MPI.COMM_WORLD.Get_size()

    #Distribute points
    #Simple-minded method to distribute equally if the division isn't even.
    chunks=numPoints%nprocs
    myNumPoints=[numPoints//nprocs+1]*chunks+[numPoints//nprocs]*(nprocs-chunks)

    tic=MPI.Wtime()
    myInside=np.ones(1)*throw(myNumPoints[myrank])
    allInside=np.zeros(1)
    MPI.COMM_WORLD.Reduce(myInside, allInside, op=MPI.SUM, root=0)
    ppi=4.*allInside/float(numPoints)
    toc=MPI.Wtime()
    if myrank==0:
        print(ppi[0])
        print("Time on "+str(nprocs)+" cores:"+str(round(toc-tic,4)))

if __name__=="__main__":
    main()
