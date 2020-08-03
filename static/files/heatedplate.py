import sys
import numpy as np
# check number of parameters and read in epsilon

argv=sys.argv
if  len(argv) > 2:
   try:
      epsilon=float(argv[1])
      filename=argv[2]
   except:
      "Illegal input"
      sys.exit(1)
else:
   print'USAGE: <epsilon> <output-file>'
   sys.exit(1)

print 'running until the difference is < ', epsilon
diff = epsilon

#In general we would read in N but let's just set it for now.
N=500

#The global array is square
nr=N
nc=N

u=np.zeros((nr+2,nc+2))
w=np.zeros((nr+2,nc+2))

# Set boundary values and compute mean boundary value. 
#This has the ice bath on the top edge, not the bottom edge.

u[:,0]=100.
u[:,nc+1]=100.
u[nr+1,:]=100.

#Initialize interior values to the boundary mean.  This value really
#doesn't matter much, it just affects the convergence rate somewhat.
mean = sum(u[:,0]) + sum(u[:,nr+1]) + sum(u[0,:]) + sum(u[nr+1,:])
mean = mean / (4.0 * (N+2))

u[1:nr,1:nc]=mean

# Compute steady-state solution
iterations=0
while ( diff >= epsilon ):
   #for i in range(1,N+1):
   #    for j in range(1,N+1):
   #        w[i,j] = 0.25*(u[i-1,j] + u[i+1,j] + u[i,j-1] + u[i,j+1])
   w[1:-1,1:-1]=0.25*(u[:-2,1:-1]+u[2:,1:-1]+u[1:-1,:-2]+u[1:-1,2:])
   diff=np.max(np.abs(w[1:-1,1:-1]-u[1:-1,1:-1]))
   if diff>=epsilon:
      u[1:N+1,1:N+1]=w[1:N+1,1:N+1].copy()
   iterations+=1

print 'completed; iterations = ', iterations

# Write solution to output file
fout = open (filename,'w')
print  >> fout, N, N
for i in range(1,N+1):
    line=" ".join(map(str,list(u[i,1:N+1])))
    row=line+"\n"
    fout.write(row)

# All done!
print  "wrote output file ", filename
