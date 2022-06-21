"""
Created on Mon Feb 3 13:11:24 2020

Demonstration of efficient use of built-in numpy array functions.

@author: Katherine Holcomb
"""

import numpy as np

def calculate(a):
    # add 3
    a+=3
    # calculate sum
    sum_A=a.sum()
    return sum_A
    
if __name__ == "__main__":
    array=np.zeros((1024,1024),dtype=int)
    sum_A = calculate(array)     
    print(sum_A)
