#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 3 13:54:52 2020

@author: Katherine Holcomb
"""

def f(x):
    return x**2-x
    
def integrate_f(a, b, N):
    s = 0
    dx = (b-a)/N
    for i in range(N):
        s += f(a+i*dx)
    return s * dx

if __name__ == "__main__":
    for i in range(2,8):
        print (integrate_f(5.0,1.0,10**i))
