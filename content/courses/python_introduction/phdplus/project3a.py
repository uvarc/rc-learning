import numpy as np
import matplotlib.pyplot as plt

def gaussian(x,m=0,s=1):
    """Compute the Gaussian function for mean m and std s"""
    y=-0.5*((x-m)/s)**2
    c=1./(np.sqrt(2.*np.pi)*s)
    return c*np.exp(y)

x=np.arange(-5.,5.01,.01)

g=gaussian(x)
plt.plot(x,g)

g=gaussian(x,2.5,1.)
plt.plot(x,g)

g=gaussian(x,s=2)
plt.plot(x,g)

plt.show()
