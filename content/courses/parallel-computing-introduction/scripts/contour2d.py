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
for row in range(nrows):
    subdomains=[]
    for col in range(ncols):
        thisrow=base+str(row)+str(col)
        print(thisrow)
        data=np.loadtxt(thisrow,unpack=False)
        subdomains.append(data)
        rowimage=np.hstack(subdomains)
    image=np.vstack(rowimage)

fig=plt.figure()
plt.contourf(image)
plt.colorbar()
plt.show()
