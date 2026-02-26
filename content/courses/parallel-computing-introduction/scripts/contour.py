import sys
import os
import argparse
import re
import numpy as np
import matplotlib.pyplot as plt

parser = argparse.ArgumentParser()
parser.add_argument("-f", "--fortran", help="Fortran ordering", action="store_true")
parser.add_argument("filename", help="Base name for data files")
args = parser.parse_args()
base = args.filename

files = [f for f in os.listdir('.') if re.match(base+r"\d*",f)]
files.sort()

subdomains=[]
for file in files:
    data=np.loadtxt(file,unpack=False)

    if args.fortran:
        data.T
    else:
        pass
    subdomains.append(data)

if args.fortran:
    image=np.hstack(subdomains)
else:
    image=np.vstack(subdomains)

fig=plt.figure()
plt.contourf(image)
plt.colorbar()
plt.show()
