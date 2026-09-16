from omp4py import *
import numpy as np

@omp
def parparfor(M):
   a=np.array([0,2,4,6,1,2,3,4,5,6])
   b=np.array([9,4,8,8,4,6,7,8,9,3])
   c=np.zeros(M)

   with omp("parallel"):
       for i in range(M):
           low = a[i]
           high = b[i]

           if low > high:
              print (f"Exiting {i}")
              break

           j=0
           with omp("for private(j)"):
               for j in range(low,high+1):
                   print(i,c[i])
                   c[i] -= a[j]/b[j]

   for i in range(M):
       print (f"i={i} c[i]={c[i]}")

parparfor(10)

