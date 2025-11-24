import sys
import numpy as np

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

   if len(sys.argv)==2:
      nsamps=int(float(sys.argv[1]))
   else:
      print("Usage: number of samples")
      exit()

   # Define the parameters to test
   xlo=-10.*np.pi; xhi=10.*np.pi
   ylo=-10.*np.pi; yhi=10.*np.pi

#We would usually have a lot of x,y points so cannot create z as a function
#of x and y, so have to use a loop
   zmax=0.
   for i in range(nsamps):
       for j in range(nsamps):
           xval=np.random.uniform(xlo,xhi)
           yval=np.random.uniform(ylo,yhi)
           zval=surface(xval,yval)
           if zval>zmax:
               zmax=zval
               xmax=xval
               ymax=yval
               max_xind=i
               max_yind=j

   print(f"Result is {zmax:.4f} at x,y= {xmax:.4f},{ymax:.4f} ")
