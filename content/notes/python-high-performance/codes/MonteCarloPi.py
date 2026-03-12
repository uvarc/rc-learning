"""
Created on Mon Feb 3 14:41:44 2020

 This program estimates the value of PI by running a Monte Carlo simulation.

 NOTE:  This is not how one would normally want to calculate PI, but serves
 to illustrate the principle.

@author: Katherine Holcomb
"""

import sys
import random

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
    # parse number of points from command line. Try 10^8
    if (len(sys.argv)>1):
        try:
            numPoints=int(float((sys.argv[1])))
            print('Pi (approximated): {}'.format(pi(numPoints)))
        except:
            print("Argument must be an integer.")
    else:
        print("USAGE:python MonteCarlo.py numPoints")


if __name__=="__main__":
    main()
