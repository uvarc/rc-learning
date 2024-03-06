import sys
import numpy as np

nrl=4
ncl=4

nrows=nrl+2
ncols=ncl+2
u=np.zeros((nrows,ncols),dtype='float')

for i in range(nrows):
    for j in range(ncols):
        u[i,j]=i+j*2

buf=np.zeros(nrows)
for i in range(nrows):
    buf[i]=u[i%ncols,0]

#Make sure it works

for i in range(nrows):
    for j in range(ncols):
        sys.stdout.write(str(u[i,j])+" ")
    sys.stdout.write("\n")

print("Buffer")
for i in range(nrows):
    sys.stdout.write(str(buf[i])+" ")
sys.stdout.write("\n")
