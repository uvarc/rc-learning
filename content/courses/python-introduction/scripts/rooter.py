#from __future__ import print_function, division #Python 2.7
from math import sqrt

def MySqrt(x):
    """Babylonian method."""
    my_sqrt=x/2.   
    s0=x
    tol=1.e-12   
    while abs(my_sqrt-s0)>tol:
        s0=my_sqrt     
        my_sqrt=0.5*(my_sqrt+x/my_sqrt)
    return my_sqrt

def relerr(x1,x2):   
    return abs((x2-x1)/x2)

def main():
    print("{:s}{:s}{:s}".format("x".center(20),"sqrt".center(10),"rel_error".rjust(14)))
    N=5
    for i in range(-N,N+1):
        x=10.0**(-i)
        print("{:14.3e}{:14.3e}{:15.7e}".format(x,sqrt(x),relerr(MySqrt(x),sqrt(x))))

if __name__=="__main__":
    main()
