import numpy as np
import sys

filename=sys.argv[1]

fh=open(filename)
x=np.fromfile(fh,dtype='int')

print(type(x),x.shape,x.size)
print(np.count_nonzero(x))
print(x)
