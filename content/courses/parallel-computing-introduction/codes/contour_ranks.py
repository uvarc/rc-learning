import sys
import argparse
import glob
import numpy as np
import pylab as plt

parser = argparse.ArgumentParser()
parser.add_argument("-f", "--fortran", help="Fortran ordering", action="store_true")
parser.add_argument("filename", help="base name of the output files")
args = parser.parse_args()
base = args.filename
endpos=len(base)

files=glob.glob(base+"*")
numbers=[]
suffixes=[]
for file in files:
    suffix=file[endpos:]
    numbers.append(int(suffix))
    suffixes.append(suffix)

pvec = np.argsort(numbers)

arrays=[]
for n in pvec:
    fname=files[n]
    arrays.append(np.loadtxt(fname,unpack=False))

if args.fortran:
    data=np.concatenate(arrays,axis=1)
else:
    data=np.concatenate(arrays,axis=0)

fig=plt.figure()
plt.contourf(data)
plt.colorbar()
plt.show()
