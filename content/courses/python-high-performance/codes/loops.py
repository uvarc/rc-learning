"""
Created on Mon Feb 03 13:10:29 2020

Demonstration of the inefficiency of for loops on an array of numbers.

@author: Katherine Holcomb
"""

import numpy as np

def calculate(a):
    # add 3
    x,y = a.shape
    for i in range(x):    
        for j in range(y):       
             array[i,j]+=3
    # calculate sum
    sum_A=0
    for i in range(x):    
        for j in range(y):       
             sum_A+=array[i,j]
    return sum_A
    
if __name__ == "__main__":
    array=np.zeros((1024,1024),dtype=int)
    sum_A = calculate(array)
    print(sum_A)
