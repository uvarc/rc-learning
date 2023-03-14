import sys
import numpy as np
import matplotlib.pyplot as plt

def surface(x,y): 
   """This is the main processing function.  We use a ufunc."""
   mu1=np.sqrt(2.0)
   mu2=np.sqrt(np.pi)
   sig1=3.1
   sig2=1.4
   z1=0.1*np.sin(x)*np.sin(x*y)
   a=(x-mu1)**2/(2*sig1**2)
   b=(y-mu2)**2/(2*sig2**2)
   z2=np.exp(-(a+b))/(sig1*sig2*np.sqrt(2.0*np.pi))
   z=z1+z2
   return z

if __name__ == '__main__':

   # Define the parameters to test
   xlo=-10.*np.pi; xhi=10.*np.pi
   ylo=-10.*np.pi; yhi=10.*np.pi

   nsamps=50
   xvals=np.random.uniform(xlo,xhi,nsamps)
   yvals=np.random.uniform(ylo,yhi,nsamps)

   x,y=np.meshgrid(xvals,yvals)
   zvals=surface(x,y)
   fig, ax = plt.subplots(subplot_kw={"projection": "3d"})
   ax.plot_surface(xvals,yvals,zvals)
   plt.show()
