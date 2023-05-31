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

    comm = MPI.Comm.Get_parent()
    myrank = comm.Get_rank()
    nprocs = comm.Get_size()

    #Get number of throws for each worker from parent
    NumPoints=np.zeros(1,dtype='int')
    comm.Bcast([NumPoints,MPI.INT],root=0)

    myNumPoints=NumPoints[0]
    myInside=np.ones(1)*throw(myNumPoints)
    myPi=np.array([4.*myInside/float(myNumPoints)])
    Pi = np.zeros(1,dtype='float')
    comm.Reduce(myPi, Pi, op=MPI.SUM, root=0)

    comm.Disconnect()

if __name__=="__main__":
    main()
