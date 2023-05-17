"""
 This program estimates the value of PI by running a Monte Carlo simulation.

 NOTE:  This is not how one would normally want to calculate PI, but serves
 to illustrate the principle.
"""

import sys
import os
import math
import random
import numpy as np
import time
from multiprocessing import Pool
from functools import reduce

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

    ncpus=int(os.getenv('NUM_PROCS'))
    print ('ncpus={}'.format(ncpus))
    chunks=numPoints%ncpus
    myNumPoints=[numPoints//ncpus+1]*chunks+[numPoints//ncpus]*(ncpus-chunks)
    print ('Points:', myNumPoints)

    pool = Pool(processes=ncpus)
    tic=time.time ()
    results = pool.map(pi,myNumPoints)
    ppi=reduce(lambda x,y:x+y,results)/ncpus
    print(ppi)
    toc=time.time ()
    pool.close(); pool.join()
    print("Parallel time on "+str(ncpus)+" cores:"+str(round(toc-tic,4)))

    #For comparison, run in serial
    tic=time.time()
    spi=pi(numPoints)
    print(spi)
    toc=time.time()
    print("Serial time:"+str(round(toc-tic,4)))

if __name__=="__main__":
    main()
