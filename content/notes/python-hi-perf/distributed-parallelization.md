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

Nearly all recent computers, including personal laptops, are multicore systems.  The central-processing units (CPUs) of these machines are divided into multiple processor cores.  These cores share the main memory (RAM) of the computer and may share at least some of the faster memory (cache).  This type of system is called a **shared-memory processing** or **symmetric multiprocessing** (SMP) computer.  

{{< figure src="/notes/python-hi-perf/SMP.png" caption="Schematic of an SMP system" >}}

A computing cluster consists of a group of computers connected by a network.  High-performance clusters nearly always have a network that is faster than the Ethernet used by consumer devices.  Many use a network called InfiniBand.  A cluster is thus a **distributed-memory processor** (DMP).  Each computer, usually called a **node**, is independent of the others and can exchange data only through the network.

{{< figure src="/notes/python-hi-perf/SMP.png" caption="Schematic of a DMP system" >}}

Multiprocessing works only on a single computer with multiple computing cores (SMP). If you have access to a computing cluster you can use distributed parallelization to run your program on multiple computers (DMP) as well as multiple cores per computer.  This requires a communications library.  The most widely used communications library is MPI, the Message Passing Interface.  
MPI works on multicore systems as well as multinode, but the programming model is still different from threads.

In MPI each process has an ID called its _rank_.  Ranks are numbered from 0 to _n-1_, for _n_ processes.  Each process runs completely independently.  No process shares memory with any other process whether running on the same node or not.  All communications occur over the network. 
To use MPI the programmer must manage the distribution of the data to different processes and the communication among the processes.  

MPI messages are identified by an "envelope" of metadata. This consists of the _destination_, the _source_ (the "return address"), a _communicator_ (a group of processes that will be exchanging information), and optionally a _tag_.  A communicator consisting of all processes, called COMM_WORLD, is set up when the MPI program is initiated.

The process with rank 0 is usually called the **root process**.  Since the mimumum number of processes is 1, the root process is the only one that is guaranteed to be present.  For this reason it is usually used to manage various bookkeeping tasks as well as input/output.

## The mpi4py Package

The most popular way to use MPI with Python is the `mpi4py` package.  It is not included in the base Anaconda distribution.  Most clusters will provide a version of Anaconda or another Python that provides mpi4py.

MPI consists of dozens of functions, though most programmers need only a fraction of the total.  The mpi4py package has implemented most of them using a "Pythonic" syntax, rather than the more C-like syntax used by other languages.  One peculiarity of mpi4py is that only particular types may be communicated; in particular, only NumPy NDArrays or pickled objects are supported.  To simplify our discussion, we will ignore the versions for pickled objects.  The requirement that NumPy arrays be sent means that even single values must be represented as one-element NumPy arrays.

MPI requires more advanced programming skills so we will just show an example here.  Our Monte Carlo pi program is well suited to MPI so we can use that.
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

The first invocation of MPI is the call to Get_rank.  This returns the rank of the process that calls it.  Remember that each MPI process runs as a separate executable; the only way their behaviors can be controlled individually is through the rank.  This call also initializes MPI; a separate MPI.Init is not required. The next line allows us to find out how many processes are in COMM_WORLD.  The number of processes for MPI programs is always set outside the program, and should never be hardcoded into the source code.

Next we divide up the number of "throws" into roughly equal chunks, just as we did in the corresponding Multiprocessing example.  The same list is generated, but we use _myrank_ to select the element of the list that each rank will use.

After this each process invokes the `pi` routine using `myNumPoints` for its rank.  We need to collect all the estimates and average them.  The _reduction_ collects the results from each rank (in rank order) and applies the "op" (operation).  A reduction must be a binary operation that returns another object of the same type; the operation is applied along the sequence.  In this case we want the sum so we add the result from rank 0 to that from rank 1, then we take that sum and add the result from rank 2, and so forth.  Dividing by the number of processes provides the average.  

The Reduce function returns each result to the process regarded as the root, which would normally be 0 as it is in this case, and root carries out the operation.  Thus _only_ the root process knows the final result.  We then select root to print out the value.

This example reruns the problem without distributing the throws in order to obtain a serial time for comparison purposes.  Of course, a real application would not do that.


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
