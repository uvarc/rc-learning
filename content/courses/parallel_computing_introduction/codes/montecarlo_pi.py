"""
 This program estimates the value of PI by running a Monte Carlo simulation.
 NOTE:  This is not how one would normally want to calculate PI, but serves
 to illustrate the principle.
"""

import sys
import os
import math
import numpy as np

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

    numInside=throw(numPoints)
    ppi=4.*numInside/float(numPoints)
    print(ppi)

if __name__=="__main__":
    main()
