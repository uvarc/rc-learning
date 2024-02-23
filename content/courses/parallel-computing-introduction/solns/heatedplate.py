import sys
import time
import numpy as np
# check number of parameters and read in parameters

#Better to use argparse but this is closer to compiled language code and simpler
#if one hasn't used argparse.
argv=sys.argv
if  len(argv) > 2:
   try:
      epsilon=float(argv[1])
      filename=argv[2]
   except:
      "Illegal input"
      sys.exit(1)
   if len(argv) == 3:
       N=500
       M=500
   if len(argv) == 4:
       try:
          N=int(argv[3])
          M=N
       except:
           "Cannot convert grid dimension to integer"
           sys.exit(2)
   if len(argv) == 5:
       try:
           N=int(argv[3])
           M=int(argv[4])
       except:
           "Cannot convert grid dimension to integer"
           sys.exit(2)
else:
   print('USAGE: epsilon output-file <N> <M>')
   sys.exit(1)

print(f'Running until the difference is < {epsilon:} with size {N:}x{M:}')

#Initialize comparison
diff = epsilon

#Set max iterations. 10 million should do it.
maxiter=10000000

#Set size of arrays
nr=N
nc=M

u=np.zeros((nr+2,nc+2))
w=np.zeros((nr+2,nc+2))

# Set boundary values and compute mean boundary value. 
#This has the ice bath on the top edge, not the bottom edge.
u[:,0]=100.
u[:,nc+1]=100.
u[nr+1,:]=100.

#Initialize interior values to the boundary mean.  This value really
#doesn't matter much, it just affects the convergence rate somewhat.
#Using all zeroes would be fine.
mean = sum(u[:,0]) + sum(u[:,nr+1]) + sum(u[0,:]) + sum(u[nr+1,:])
mean = mean / (4.0 * (N+2))

#Be sure not to overwrite the boundary values
u[1:nr+1,1:nc+1]=mean

# Compute steady-state solution
iterations=0
diff_interval=1

#Arbitrary, just to make sure we enter loop
diff=10*epsilon

start_time=time.time()

while ( diff >= epsilon ):
#   for i in range(1,nr+1):
#       for j in range(1,nc+1):
#           w[i,j] = 0.25*(u[i-1,j] + u[i+1,j] + u[i,j-1] + u[i,j+1])
#  It is approximately a factor of 100 faster to use numpy array operations.
   w[1:-1,1:-1]=0.25*(u[:-2,1:-1]+u[2:,1:-1]+u[1:-1,:-2]+u[1:-1,2:])

   if iterations%diff_interval==0:
       diff=np.max(np.abs(w[1:-1,1:-1]-u[1:-1,1:-1]))
       if diff<=epsilon:
           break
   if iterations>maxiter:
           print("Warning: maximum iterations exceeded")
           break
   u[1:nr+1,1:nc+1]=w[1:nr+1,1:nc+1].copy()
   iterations+=1

print(f'completed in {iterations:} iterations with time {time.time()-start_time:.2f}')

# Write solution to output file
fout = open (filename,'w')
for i in range(1,N+1):
    line=" ".join(map(str,list(u[i,1:M+1])))
    row=line+"\n"
    fout.write(row)

# All done!
print(f"wrote output file {filename:}")
