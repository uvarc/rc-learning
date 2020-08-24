---
title: "High Performance Programming in Python"
type: article
toc: true
---

# Overview

**First Things First:** Always start with a working, correct code. Make it as clean and readable as possible.

For this workshop, it is assumed that you have experience with programming in Python. We will explore examples for different **Optimization Strategies**, including

* [Serial Optimization:](#serial-optimization-strategies) Replacing inefficient code constructs with more efficient ones (single process).
* [Multiprocessing:](#multiprocessing) Executing multiple processes on a single computer (shared memory).
* [Distributed Parallelization:](#distributed-parallelization) Executing multiple processes across multiple computers (distributed memory, HPC cluster).

- - -

# Setup for this Workshop

To follow along for the [Serial Optimization](#serial-optimization-strategies) and [Multiprocessing](#multiprocessing) examples, you can execute the code examples on your own computer or on Rivanna.  Examples described in the last section, [Distributed Parallelization](#distributed-parallelization), have to be executed on UVA's high-performance computing platform, Rivanna.

All UVA workshop participants have been provisioned accounts on Rivanna. 

We recommend the Anaconda distribution (<a href="https://www.anaconda.com/distribution/" target="balnk_">download</a>) to run the code examples. Anaconda provides multiple Python  versions, an integrated development environment (IDE) with editor and profiler, and an easy to use package environment manager.

**Before we move on, please follow these steps to verify that your Rivanna account is active:**

### Check your Access to Rivanna

1. In your webbrowser, got to <a href="https://rivanna-desktop.hpc.virginia.edu" target="_blank">rivanna-desktop.hpc.virginia.edu</a>.  This takes you to our FastX web portal that lets you launch a remote desktop environment on Rivanna.  If you are off Grounds, you need to be connected through the UVA Anywhere VPN client.

2. Log in with your UVA credentials and start a MATE session.  You can find a more detailed description of the Rivanna procedure login <a href="https://www.rc.virginia.edu/userinfo/rivanna/logintools/fastx/" target="_blank">here</a>.
	* **User name:** Your UVA computing id (e.g. mst3k; don't enter your entire email address)
	* **Password:** Your UVA Netbadge password 

3. Starting Spyder (Anaconda's IDE): Open an terminal window and type
 ```
module load anaconda
python -V
```
Python 3.6.6

```
spyder &
```
<br>

### Get the example scripts 
	
You can copy the example scripts to your Rivanna home directory or download them to your computer. **The code examples are provided for Python 3.** 


* **On Rivanna:** Open a terminal window and run this command to copy the examples to your home directory.  Then change to the directory with the scripts:
```bash
cp -r /share/resources/tutorials/python-hi-perf ~/
cd ~/python-hi-perf
```
      
* **Your computer:**  [Download examples](/scripts/python-hi-perf/python-hi-perf.zip) as an .zip archive.


- - -

# Profiling & Timing

Profiling can help us find a program's bottlenecks.  Keep in mind that most profilers work per function.  If you do not have your code well separated into functions, the information will be less useful.

### Python Profiler in the Spyder IDE
The Anaconda Spyder IDE provides an integrated Profiler that is easy to use. To profile your Python code follow these steps:

1. Open the Python script in the Editor pane.  If your program is comprised of multiple files, open the script that contains the the main function.
2. In the Spyder menu, go to `Run` -> `Profile`.

The results will be shown in the `Profiler` pane. 

**Exercise:** 
Open the `fibonacci.py` file and execute it with the Spyder Profiler.  The code uses an inefficient algorithm on purpose.  Let's look at the output in the `Profiler` pane.  What function was called most frequently and has the largest cumulative run time?

![](/images/python-hi-perf/fibonacci-profiler.png)

Profilers are statistical in nature.  They query the program to find out what it is doing at a snapshot during the run, then compile the results into a kind of frequency chart.  A more detailed description of the Profiler can be found <a href="http://docs.spyder-ide.org/profiler.html" target="_blank">here</a>.


### Using the cProfile and pstats Modules in your script

Python includes the `cProfile` and `profile` packages.  In Python 2, profile is a pure Python module with significant more overhead than the C extensions of cProfile (<a href="https://docs.python.org/2/library/profile.html" target="_blank">read the details</a>).  In Python 3, `profile` is `cProfile` by default.  These packages can be used from the command line or in your Python scripts.

You can use the `cProfile` and `pstats` modules in your script to profile specific functions and save the profiling results to a file. Let's assume that we have a module `MyModule` that defines a function `myFunc` to profile: 
```
import MyModule
import cProfile
cProfile.runctx("MyModule.myFunc()", globals(), locals(), "Profile.result") 
```
Here `globals()` defines a set of global variables to be set for the profiling run.  The `locals()` is a dictionary for arguments passed to the specific function to be profiled, i.e. `MyModule.myFunc`. The last argument, `Profile.result`, defines the file that the profiling output is written to.

The following code snippet can be used to implement profiling of the `fib_seq` function in the `fibonacci.py` script.
```
import cProfile,pstats

cProfile.runctx("fib_seq", globals(), {'n':20}, "Profile.result")
s = pstats.Stats("Profile.result") 
s.strip_dirs().sort_stats("time").print_stats()
```
Note the passing of `{'n':20}` to pass local arguments to the `fib_seq` function. The profiling stats are read from the `Profile.result` file, the path information of the involved Python modules is removed, and the output is sorted by `time` values before it is printed.


### Timing

Often we want to know exactly how long a particular code, or portion of the code, ran.  Profiling adds some execution overhead.  Instead we must measure the elapsed time directly.

The `time` module is available for longer segments of code.

```
import time
start_time=time.clock()
# do stuff
end_time=time.clock()
print ("Elapsed time is ",end_time-start_time)
```

Python 3 offers the `timeit` module for short snippets.
```
import timeit
print(timeit.timeit('"-".join(str(n) for n in range(100))', number=10000))
```
Since a single repetition of a command in Python may run too quickly to measure easily, we specify a number of repetitions (`number=10000`).  

- - -

# Serial Optimization Strategies

1. Profile or time your code.
2. Tune the most time-intensive section.
3. Did performance increase?
	Yes: go to second most time-consuming code
	No: try a different solution for the slowest code

### Avoid for Loops

Convert everything you can to use NumPy array intrinsics.

**Example 1:** Nested for loops -- **very inefficient** (`loops.py`)
```
import numpy as np

def calculate(a):
    # add 3
    x,y = a.shape
    for i in range(x):    
        for j in range(y):       
             array[i,j]+=3
    # calculate sum
    sum_A=0
    for i in range(x):    
        for j in range(y):       
             sum_A+=array[i,j]
    return sum_A
    
if __name__ == "__main__":
    array=np.zeros((1024,1024),dtype=int)
    sum_A = calculate(array)
    print(sum_A)
```

**Example 2:** No for loops -- **very efficient** (`aops.py`)
```
import numpy as np

def calculate(a):
    # add 3
    a+=3
    # calculate sum
    sum_A=a.sum()
    return sum_A
    
if __name__ == "__main__":
    array=np.zeros((1024,1024),dtype=int)
    sum_A = calculate(array)     
    print(sum_A)
```

**Results** with Python 3.6.9 on one particular system:

| Program | total time: calculate(a)
| --- | --- |
| loops.py | 1.197 sec |
| aops.py | .211 sec |

<br>
**Extreme Example**
```
# assume numpy array with n x n elements
for i in range(1,n-1):
      for j in range(1,n-1):
           u[i,j]=0.25*(u[i+1,j]+u[i-1,j]+u[i,j+1]+u[i,j-1])
```
Replace with a single line
```
u[1:-1,1:-1]=0.25*(u[2:,1:-1]+u[:-2,1:-1]+u[1:-1,2:]+u[1:-1,:-2] 
```

**Other ways to avoid loops**

* <a href="https://docs.python.org/3/tutorial/datastructures.html#list-comprehensions" target="_blank">List comprehensions</a>
* <a href="https://docs.python.org/3/library/itertools.html?highlight=map%20reduce" target="_blank">map/reduce/itertools</a>

### Avoid Copying

**Bad:**
```
s = ""
for x in mylist:
    s += string_function(x)
```

**Better:**
```
slist = [string_function(el) for el in mylist]
s = "".join(slist)
```

Not only does the first version have a for loop, but since strings are immutable each concatenation requires copying.  A join is much faster.  
**Note:** string concatenation is faster in Python 3 than in Python 2.

### Minimize Use of Dynamically Sized Objects

Use dynamically sized objects when appropriate, but do not append if you don’t have to do so.  Especially avoid inserting.

### Use Functions

Due to some quirks of Python, functions are faster than straight code.  

This implies you should use a main() function even if you never import your file as a module:

```
def main():
     solve_problem()

if __name__==”__main__”:
      main()
```

### Wrapping Compiled Code in Python

* If you have Fortran you can use f2py
* Part of NumPy
* Can work for C as well
* Extremely easy to use
* Can wrap legacy F77 and newer F90+ (with modules)
* Must be used from the command line
* A module can be imported as f2py2e

http://docs.scipy.org/doc/numpy-dev/f2py/

SWIG can work for C/C++; SIP is good for C++

### Cython

Cython is a package that allows Python code to be compiled into C code.  Some rewriting is required because C requires statically-typed variables.  Cython defines two additional keywords to declare functions, `cdef` and `cpdef`. `cdef` is basically C and can produce the fastest code, but `cdef` declared functions are not visible to Python code that imports the module. `cpdef` is mix of C with dynamic bindings for passing of Python objects which makes it slower than `cdef` (<a href="https://notes-on-cython.readthedocs.io/en/latest/function_declarations.html" target="_blank">read the details</a>).

**Example:** `integrate.py` 

Suppose we start with
```
def f(x):
    return x**2-x
    
def integrate_f(a, b, N):
    s = 0
    dx = (b-a)/N
    for i in range(N):
        s += f(a+i*dx)
    return s * dx
```

We can speed this up with
```
cpdef double f(double x):
    return x**2-x

cpdef double integrate_f(double a, double b, int N):
    cdef int i
    cdef double s, dx
    s = 0
    dx = (b-a)/N
    for i in range(N):
        s += f(a+i*dx)
    return s*dx
```

Save the above code as `integrate_cyf.pyx`.  Now create a `setup.py` file:
```
from distutils.core import setup
from Cython.Build import cythonize

setup(
	ext_modules = cythonize("integrate_cyf.pyx")
)	
```

On the command line run `python setup.py build_ext --inplace` to build the extension.

This will create a file `integrate_cyf.c` in your local directory. In addition you will find a file called `integrate_cyf.so` in unix or `integrate_cyf.pyd` in Windows. Now you can import your module in your Python scripts.
```
import integrate_cyf as icyf
print(icyf.integrate_f(1.,51.,1000))
```

More detailed information describing the use of Cython can be found <a href="https://cython.readthedocs.io/en/latest/src/tutorial/cython_tutorial.html" target="_blank">here</a>.

### Numba

Numba is available with the Anaconda Python distribution.   It compiles selected functions using the LLVM compiler.  Numba is accessed through a decorator.  Decorators in Python are wrappers that modify the functions without the need to change the code.

**Example:**
A well-known but slow way to compute pi is by a Monte Carlo method.  Given a circle of unit radius inside a square with side length 2, we can estimate the area inside and outside the circle by throwing “darts” (random locations).  Since the area of the circle is pi and the area of the square is 4, the ratio of hits inside the circle to the total thrown is pi/4.  

Open the `MonteCarloPi.py` script.
```
import sys
import random

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
    # parse number of points from command line. Try 10^8
    if (len(sys.argv)>1):
        try:
            numPoints=int(float((sys.argv[1])))
            print('Pi (approximated): {}'.format(pi(numPoints)))
        except:
            print("Argument must be an integer.")
    else:
        print("USAGE:python MonteCarlo.py numPoints")


if __name__=="__main__":
    main()
```

Running with 10^9 points takes 6 minutes and 21 seconds on one particular system.

Now add another `import` statement.
```
from numba import jit
```
Add the decorator above the pi function:
```
@jit
def pi(numPoints):
```
No other changes are required.  The time is reduced to only 14.7 seconds!

### Concluding Advice for Serial Optimization

* Do not sacrifice readability for optimization.  Human time is much more expensive than computer time.
* But be mindful of efficiencies as you code (e.g. think about looping correctly for your language as you go).
* Do simple optimizations first (such as straightforward loop reordering and mathematical reorganization).  Profile before undertaking extensive optimization efforts.
* Use timing functions to obtain finer-grained information about bottlenecks as needed.

- - -

# Multiprocessing

Sometimes you cannot sufficiently speed up your program even with all optimization tricks.  You may be able to take advantage of modern multicore processors to distribute the work across different cores.

### The Global Interpreter Lock (GIL)

Standard Python implements a GIL (global interpreter lock). Threads cannot be started within a single interpreter.

Threads can be faked but that’s slow.  Better to just start another Python process.  The `multiprocessing` package handles this and manages communication between the processes.  For the purpose of this workshop we will experiment with a few different multiprocessing approaches. A detailed description can be found on the official <a href="https://docs.python.org/3/library/multiprocessing.html" target="_blank">Multiprocessing website</a>.

**Import the package**
```
from multiprocessing import Process
```

**Define a function**
```
def f(name): 
    print('hello from '+name)

if __name__ == '__main__': 
    ncpus=2
    for i in range(ncpus):
        p=Process(target=f,args=('str(i)',)) 
        p.start()
    p.join() # wait for all processes to be done
```

**Note that `multiprocessing` requires a main() function and must be run inside it.**  This means that some examples, such as the multiprocessing.Pool examples will not work in the interactive interpreter. 

For manager-worker problems, we can start a pool of workers.
```
from multiprocessing import Pool 

def f(x): 
  return x*x 

if __name__ == '__main__': 
   pool = Pool(processes=4) 
   result_handle = pool.apply_async(f, [10]) 
   # Do other stuff
   
   # Now check for result
   print(result_handle.get(timeout=1))  
```

In this example we created a Pool of four workers (`Pool(processes=4)`). The `pool.apply_async` call submits a workload to the Pool of workers.  The first parameter is the name of the function, in this case `f`, and the second argument defines the arguent(s) that need to be passed to the specified function `f`.  Here it is a list with a single element(`[10]`) since the `f` function expects a single argument.

**Note that the `apply_asynchronous` call is not blocking and code execution continues before the result is calculated.** The returned object `result_handle` is a handle to access the result at a later timepoint.  This allows us to submit other workloads to the Pool or execute other code blocks while the calculation in `f` is ongoing.  To get the actual result we call the `result_handle.get()` function.  This call is blocking and we can define a timeout, in this case 1 sec (`timeout=1`).

<br>
**The Pool Class**

You can also define a pool of workers using an instance of the `Pool` class.  
```
from multiprocessing import Pool 

def f(x): 
  return x*x 

if __name__ == '__main__': 
   pool = Pool(processes=4) 
   
   # Submit 10 function calls to Pool and map results   
   print(pool.map(f, range(10))) 
```

<br>
Here is a more realistic example.  Let’s parallelize our Monte Carlo pi solver (`MonteCarloPiMC.py`).
```
"""
 This program estimates the value of PI by running a Monte Carlo simulation.

 NOTE:  This is not how one would normally want to calculate PI, but serves
 to illustrate the principle.
"""

import sys
import os
import math
import random
import numpy as np
import time
from multiprocessing import Pool
from functools import reduce

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

    ncpus=int(os.getenv('NUM_PROCS'))
    print ('ncpus={}'.format(ncpus))
    chunks=numPoints%ncpus
    myNumPoints=[numPoints//ncpus+1]*chunks+[numPoints//ncpus]*(ncpus-chunks)
    print ('Points:', myNumPoints)

    pool = Pool(processes=ncpus)
    tic=time.time ()
    results = pool.map(pi,myNumPoints)
    ppi=reduce(lambda x,y:x+y,results)/ncpus
    print(ppi)
    toc=time.time ()
    pool.close(); pool.join()
    print("Parallel time on "+str(ncpus)+" cores:"+str(round(toc-tic,4)))

    #For comparison, run in serial
    tic=time.time()
    spi=pi(numPoints)
    print(spi)
    toc=time.time()
    print("Serial time:"+str(round(toc-tic,4)))

if __name__=="__main__":
    main()
```

Notice that we had to write some additional code to split up the points among the processes.  We also do not hard-code the number of processes, but will set an environment variable `NUM_PROCS` outside to select the core count. 

### Running the Program on Rivanna

In order to execute our program on designated compute node(s), we need to write a simple bash script that defines the compute resources we need.  We call this our job script.  For our example, the job script `pimc.sh` looks like this:

```
#!/bin/bash
#SBATCH -N 1
#SBATCH --cpus-per-task=4
#SBATCH --account=rivanna-training
#SBATCH -p standard
#SBATCH -t 10:00:00

echo Running on `hostname`

module purge
module load anaconda/5.2.0-py3.6

# set the NUM_PROCS env variable for the Python script
export NUM_PROCS=$SLURM_CPUS_PER_TASK
python MonteCarloPiMC.py 100000000
```

You can view this script in a text editor on Rivanna.  If you are connected through a FastX Mate session, got to the menu **Applications** -> **Accessories** --> **Pluma Text Editor**.

The `#SBATCH` directives define the compute resources (`-N`, `--cpus-per-task`, `-p`), compute wall time (`-t`), and the allocation account (`--account`) to be used. `-N 1` specifies that the job runs on a single node. With `--cpus-per-task` we request the number of cpu cores for the job.  By increasing the number for `--cpus-per-task` we can take advantage of multiple cpu cores and set up a bigger pool of workers. Ideally we want to match the worker pool size with the number of cpu cores.

<br>
**Submitting the job:**

The job needs to be submitted to the job scheduler with a specific command. On Rivanna we use the Simple Linux Utility Resource Manager (SLURM) and the `sbatch` command for job submission.

Open a terminal window and execute this command:
```
sbatch pimc.sh
``` 

After submission you should see output like this:
```
Submitted batch job 9024339
```
The integer number resembles a unique job id.

<br>
**Checking the Job Status:**

You can check the status of your jobs by running either one of these commands:

* `squeue -u <YOUR_ID`
* `sacct`

The `squeue` command show all active jobs, either pending or running.  The `sacct` command shows the history of your jobs whether they are pending, running, completed, cancelled or failed.

You can find more detailed about SLURM and job management on our [website](https://www.rc.virginia.edu/userinfo/rivanna/slurm).

<br>
**Checking the Output File:**
SLURM creates output files for each job that log the information that the program prints to stdout and stderror during the job run.  The file(s) will also include information in case the job run was aborted.  By default the name of the SLURM output file is `slurm-<JOB_ID>.out`.  Check the directory in which you executed the sbatch command for a SLRURM output file and open it in a text editor.

<br>
**Exercises:** Rerun the job using 2, 4, or 8 cpu cores.  In order to do this, open the `pimc.sh`, change the `--cpus-per-task` option accordingly, save the file, and resubmit the job script from the terminal window with the `sbatch` command. 

### Scaling

When we run the exercise with 10^9 points we may obtain results like these (on one particular workstation):

| CPU cores | Run time | Scaling
| --- | --- | --- |
| 1 (serial) | 402 sec | 1 |
| 4 | 109.5 sec | |
| 8 | 60.5 sec | |
| 16 | 32.5 sec | |

<br>
If we plot time versus number of cores we obtain the following graph.  The orange line is ideal scaling, where the total time is the serial time divided by the number of cores used.  The blue line shows the actual runtime and speedup achieved.

![](/images/python-hi-perf/mp-scaling.png)

Our actual scaling in this case is quite close to perfect.  This has a lot to do with our problem; the amount of time taken is mostly proportional to the number of throws to be calculated.  Not all problems scale this well.

### Combining Approaches

If we are using Anaconda we can use Numba and Multiprocessing together.  On the same workstation this reduced the serial time to 12.8 seconds and the time on 4 cores to 5.8 seconds.  The poorer scaling here could be due to the time required being so small that other tasks, called overhead, that could not be parallelized became dominant.

- - -

# Distributed Parallelization

Multiprocessing works only on a single computer with multiple computing cores. If you have access to a computing cluster you can use distributed parallelization to run your program on multiple computers as well as multiple cores per computer.  This requires a communications library.  The most widely used communications library is MPI or Message Passing Interface.  To use MPI the programmer must manage the distribution of the data to different processes and the communication among the processes.  

### The mpi4py Package

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
