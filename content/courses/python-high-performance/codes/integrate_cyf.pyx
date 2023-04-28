"""
Created on Mon Feb 3 14:05:03 2020

@author: Katherine Holcomb
"""

cpdef double f(double x):
    return x**2-x

cpdef double integrate_f(double a, double b, int N):
    cdef int i
    cdef double s, dx
    s = 0
    dx = (b-a)/N
    for i in range(N):
        s += f(a+i*dx)
    return s*dx
