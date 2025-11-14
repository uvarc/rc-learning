import sys
import numpy as np
from scipy import optimize

def surface(xy): 
   """This is the main processing function.  We use a ufunc."""
   x,y=xy
   mu1=np.sqrt(2.0)
   mu2=np.sqrt(np.pi)
   sig1=3.1
   sig2=1.4
   z1=0.1*np.sin(x)*np.sin(x*y)
   a=(x-mu1)**2/(2*sig1**2)
   b=(y-mu2)**2/(2*sig2**2)
   z2=np.exp(-(a+b))/(sig1*sig2*np.sqrt(2.0*np.pi))
   z=z1+z2
   return -z

if __name__ == '__main__':

   # Define the parameters to test
   xlo=-10.*np.pi; xhi=10.*np.pi
   ylo=-10.*np.pi; yhi=10.*np.pi

   bnds=((xlo,xhi),(ylo,yhi))
   res = optimize.minimize(surface, (0, 0), method='SLSQP', bounds=bnds)
   print(res)
