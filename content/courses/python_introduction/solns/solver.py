"""
   This program solves an equation by an extremely stupid brute-force method.
   Author:    K. Holcomb
   Changelog: 20220203: Initial version, modified from different equation
"""
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import axes3d
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
    z_max=np.max(z)
    ind = np.unravel_index(np.argmax(z),z.shape)
    best_x=x[ind]
    best_y=y[ind]
    return best_x,best_y,z_max

start_val=-10.*math.pi
end_val=10.*math.pi

Nsteps=8000

xvals=(start_val-end_val)*np.random.random(Nsteps)+start_val
yvals=(start_val-end_val)*np.random.random(Nsteps)+start_val
x,y=np.meshgrid(xvals,yvals)
z=eqn(x,y)

best_x,best_y,z_max=get_max(x,y,z)

print("The best solution to the equation is {:.4f} at x={:.4f} and y={:.4f}"\
       .format(z_max,best_x,best_y))

fig, ax = plt.subplots(subplot_kw={"projection": "3d"})
surf = ax.plot_surface(x[0:-1:100,0:-1:100],y[0:-1:100,0:-1:100],z[0:-1:100,0:-1:100])
plt.show()
