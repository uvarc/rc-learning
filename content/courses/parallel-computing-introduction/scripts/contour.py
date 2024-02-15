import sys
import argparse
import glob
import numpy as np
import pylab as plt

parser = argparse.ArgumentParser()
parser.add_argument("-f", "--fortran", help="Fortran ordering", action="store_true")
parser.add_argument("filename", help="output file name")
args = parser.parse_args()
base = args.filename

data=np.loadtxt(args.filename,unpack=False)

if args.fortran:
    data.T

print(data.size, data.shape)

fig=plt.figure()
plt.contourf(data)
plt.colorbar()
plt.show()
