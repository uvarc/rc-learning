import sys
import numpy as np

# Calculate a definite integral using trapezoid rule
# Does not use scipy so we can parallelize it
                                                                                
def f(x):
    return np.sin(x)

def trap(a,b,h,n,f):
    integral=(f(a) + f(b))/2.0
    x=a
    for i in range(n):
        x+=h
        integral+=f(x)
    integral*=h
    return integral
                                                                                
if len(sys.argv)==4:
    a=float(sys.argv[1])
    b=float(sys.argv[2])
    n=int(float(sys.argv[3]))
else:
    print("Usage: lower bound, upper bound, number of steps")
    sys.exit()

h=(b-a)/n
integral=trap(a,b,h,n,f)

print("Result ",integral)
