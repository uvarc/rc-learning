import sys
import os
import argparse
import re
import numpy as np
import matplotlib.pyplot as plt

parser = argparse.ArgumentParser()
parser.add_argument("-r", "--rows", help="Rows in process topology")
parser.add_argument("-c", "--cols", help="Columns in process topology")
#parser.add_argument("-f", "--fortran", help="Fortran ordering", action="store_true")
parser.add_argument("filename", help="Base name for data files")
args = parser.parse_args()
base = args.filename
nrows = int(args.rows)
ncols = int(args.cols)

files = [f for f in os.listdir('.') if re.match(base+r"\d*",f)]
files.sort()
imgdata={}
for row in range(nrows):
    for col in range(ncols):
        thisdata=base+str(row)+str(col)
        data=np.loadtxt(thisdata,unpack=False)
        nrvals,ncvals=data.shape
        imgdata[(row,col)]=data

rsize=nrows*nrvals
csize=ncols*ncvals
image=np.zeros((rsize,csize))

for row in range(nrows):
    lbound0=row*nrvals
    ubound0=lbound0+nrvals
    for col in range(ncols):
        lbound1=col*ncvals
        ubound1=lbound1+ncvals
        image[lbound0:ubound0,lbound1:ubound1]=imgdata[(row,col)]

fig=plt.figure()
plt.contourf(image)
plt.colorbar()
plt.show()
