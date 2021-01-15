---
title: "Distributed Parallel Programming with MPI"
type: article
toc: true
weight: 4
menu: 
    hp-python:
        parent: High-Performance Python
        weight: 4
---

Multiprocessing works only on a single computer with multiple computing cores. If you have access to a computing cluster you can use distributed parallelization to run your program on multiple computers as well as multiple cores per computer.  This requires a communications library.  The most widely used communications library is MPI or Message Passing Interface.  To use MPI the programmer must manage the distribution of the data to different processes and the communication among the processes.  

## The mpi4py Package

The most popular way to use MPI with Python is the `mpi4py` package.  It is not included in the base Anaconda distribution.  Most clusters will provide a version of Anaconda or another Python that provides mpi4py.

MPI requires more advanced programming skills so we will just show an example here.  Our Monte Carlo pi program is well suited to MPI so we can use that.

In MPI each process has an ID called its rank.  Ranks are numbered from 0.  
```
"""
 This program estimates the value of PI by running a Monte Carlo simulation.
 Imagining a virtual dart board of radius 1 (centered at the origin) and
 inscribed within a square of side-length 2 (centered at the origin), the
 program throws a large number of "darts" at the board and counts how many
 of the darts land within the circle.  Because we know that a circle of
 radius 1 has an area of PI, and because we know that a square of side-length
 2 has an area of 4, we can approximate the value of PI by considering that
 the ratio of the number of darts inside the circle to the number of darts
 outside the circle but in the square should be equal to the raio of PI over
 4.  Simplified, PI = 4 ! (dartsInside / dartsTotal).

 NOTE:  This is not how one would normally want to calculate PI, but serves
 to illustrate the principle.
"""

import sys
import os
import math
import random
import numpy as np
import time
from mpi4py import MPI

def pi(numPoints):
    """Throw a series of imaginary darts at an imaginary dartboard of unit 
        radius and count how many land inside the circle."""

    numInside=0
  
    for i in range(numPoints):
        x=random.random()
        y=random.random()
        if (x**2+y**2<1):
            numInside+=1

    pi=4.0*numInside/numPoints
    return pi

def main():

    if (len(sys.argv)>1):
        try:
            numPoints=int(float((sys.argv[1])))
        except:
            print("Argument must be an integer.")
    else:
        print("USAGE:python MonteCarlo.py numPoints")
        exit()

    myrank = MPI.COMM_WORLD.Get_rank()
    nprocs = MPI.COMM_WORLD.Get_size()

    chunks=numPoints%nprocs
    myNumPoints=[numPoints//nprocs+1]*chunks+[numPoints//nprocs]*(nprocs-chunks)

    tic=time.time ()
    mypi=np.ones(1)*pi(myNumPoints[myrank])
    ppi=np.zeros(1)
    MPI.COMM_WORLD.Reduce(mypi, ppi, op=MPI.SUM, root=0)
    ppi/=float(nprocs)
    toc=time.time()
    if myrank==0:
        print(ppi[0])
        print("Parallel time on "+str(nprocs)+" cores:"+str(round(toc-tic,4)))

    tic=time.time()
    spi=pi(numPoints)
    print(spi)
    toc=time.time()
    print("Serial time:"+str(round(toc-tic,4)))

if __name__=="__main__":
    main()
```


### Running the MPI Python Program on Rivanna

In order to launch multiple tasks (or processes) of our program, we run this program through the MPI executor.  On our HPC cluster this is srun.

```
srun python MonteCarloPiMPI.py 1000000000
```

**Note you cannot launch the MPI program with `srun` on the Rivanna login nodes.**  In order to execute our program on designated compute node(s), we need to write a simple bash script that defines the compute resources we need.  We call this our job script.  For our example, the job script `pimpi.sh` looks like this:
```
#!/bin/bash
#SBATCH -N 1
#SBATCH --ntasks-per-node=8
#SBATCH --account=rivanna-training
#SBATCH -p standard
#SBATCH -t 10:00:00

echo Running on `hostname`

module purge
module load gcc/7.1.0
module load openmpi/3.1.4
module load mpi4py/3.0.0-py3.6

srun python MonteCarloPiMPI.py 100000000
```

The `#SBATCH` directives define the compute resources (`-N`, `--ntasks-per-node`, `-p`), compute wall time (`-t`), and the allocation account (`--account`) to be used. `-N 1` specifies that all MPI tasks should run on a single node.  We are limiting the number of nodes for this workshop so that everyone gets a chance to run their code on the shared resources.  

**Submitting the job:**

Open a terminal window and execute this command:
```
sbatch pimpi.sh
``` 

<br>
**Checking the Job Status:**

Check the job status with the `squeue -u` or `sacct` commands as described in the [Multiprocessing](#multiprocessing) section. 

<br>
**Checking the Output File:**

Open the `slurm-XXXXXX.out` files in a text editor and record the total run time for the job.

<br>
**Exercises**

Rerun the MPI job using 1 or 4 cpu cores by changing the `--ntasks-per-node` option.

### Scaling
On the cluster, the timings are very similar to Multiprocessing on the workstation.

| CPU Cores | Run Time |
| --- | --- |
| 1 (serial) | 400 sec|
| 4 | 102 sec |
| 8 | 60 sec |
| 16 | 31 sec |
