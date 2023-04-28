"""
 This program estimates the value of PI by running a Monte Carlo simulation.
 Imagining a virtual dart board of radius 1 (centered at the origin) and
 inscribed within a square of side-length 2 (centered at the origin), the
 program throws a large number of "darts" at the board and counts how many
 of the darts land within the circle.  Because we know that a circle of
 radius 1 has an area of PI, and because we know that a square of side-length
 2 has an area of 4, we can approximate the value of PI by considering that
 the ratio of the number of darts inside the circle to the number of darts
 outside the circle but in the square should be equal to the raio of PI over
 4.  Simplified, PI = 4 ! (dartsInside / dartsTotal).

 NOTE:  This is not how one would normally want to calculate PI, but serves
 to illustrate the principle.
"""

import sys
import os
import math
import random
import numpy as np
import time
from mpi4py import MPI

def pi(numPoints):
    """Throw a series of imaginary darts at an imaginary dartboard of unit 
        radius and count how many land inside the circle."""

    numInside=0
  
    for i in range(numPoints):
        x=random.random()
        y=random.random()
        if (x**2+y**2<1):
            numInside+=1

    pi=4.0*numInside/numPoints
    return pi

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

    chunks=numPoints%nprocs
    myNumPoints=[numPoints//nprocs+1]*chunks+[numPoints//nprocs]*(nprocs-chunks)

    tic=time.time ()
    mypi=np.ones(1)*pi(myNumPoints[myrank])
    ppi=np.zeros(1)
    MPI.COMM_WORLD.Reduce(mypi, ppi, op=MPI.SUM, root=0)
    ppi/=float(nprocs)
    toc=time.time()
    if myrank==0:
        print("Pi is approximately",ppi[0])
        print("Parallel time on "+str(nprocs)+" cores:"+str(round(toc-tic,4)))

        tic=time.time()
        spi=pi(numPoints)
        print("Pi is approximately ",spi)
        toc=time.time()
        print("Serial time:"+str(round(toc-tic,4)))

if __name__=="__main__":
    main()
