"""
   This program solves an equation by an extremely stupid brute-force method.
   Author:    K. Holcomb
   Changelog: 20220203: Initial version, modified from different equation
"""
import numpy as np
import math

def eqn(x,y):
    m1=math.sqrt(2)
    m2=math.sqrt(math.pi)
    sig1=3.1
    sig2=1.4
    z1=0.1*np.sin(x)*np.sin(x*y)
    a=(x-m1)**2/(2.*sig1**2)
    b=(y-m2)**2/(2.*sig2**2)
    z2=np.exp(-(a+b))/(sig1*sig2*math.sqrt(2.*math.pi))
    return z1+z2

def get_max(x,y,z):
    z_max=eqn(x[0],y[0])
    ind=0
    for i in range(1,len(x)):
        z=eqn(x[i],y[i])
        if z>z_max: 
            z_max=z
            ind=i
    print(i,z_max)
    return x[i],y[i],z_max

start_val=-10.*math.pi
end_val=10.*math.pi

Nsteps=8000000

x=(start_val-end_val)*np.random.random(Nsteps)+start_val
y=(start_val-end_val)*np.random.random(Nsteps)+start_val
z=eqn(x,y)

best_x,best_y,z_max=get_max(x,y,z)

print("The best solution to the equation is {:.4f} at x={:.4f} and y={:.4f}"\
       .format(z_max,best_x,best_y))
