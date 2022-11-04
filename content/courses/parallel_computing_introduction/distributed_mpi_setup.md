---
title: "Setting Up and Running MPI"
toc: true
type: docs
weight: 23
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
        weight: 23
---

We are now ready to write our first MPI program.  Select your choice of language below for this example.

## A Hello World example.

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel_computing_introduction/codes/mpi1.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel_computing_introduction/codes/mpi1.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel_computing_introduction/codes/mpi1.py" lang="python" >}}
{{< /spoiler >}}

## Preparing and Running MPI Codes

### On a Remote Cluster

Refer to the instructions from your site, for example [UVA Research Computing](https://www.rc.virginia.edu/userinfo/howtos/rivanna/mpi-howto/) for our local environment.  Nearly always, you will be required to prepare your code and run it through a _resource manager_ such as [Slurm](https://www.rc.virginia.edu/userinfo/rivanna/slurm/).

For Python, you will need to install mpi4py.  You may wish to create a conda environment for it.  On the UVA system you must use `pip` rather than conda. 
```bash
module load gcc openmpi
module load anaconda
pip install --user mpi4py
```

### On a Local Computer

If you have access to a multicore computer, you can run MPI programs on it.

If using a compiled language, before you can build MPI codes you must install a compiler, and possibly some kind of IDE.  See our guides for [C++](/courses/cpp_introduction/setting_up) or [Fortran](/courses/fortran_introduction/setting_up).

For Python, on all operating systems install [mpi4py](https://mpi4py.readthedocs.io/en/stable/index.html). To install mpi4py you must have a working `mpicc` compiler.  For Anaconda this generally requires installing the `gcc_linux-64` package using `conda`. Then you can `conda install mpi4py`.  You may wish to create a new conda environment for MPI programming.  

#### Linux

Parallel programs are most frequently used in a _high-performance computing_ environment such as the clusters we have discussed, but many multicore workstations are available that can run Linux and many parallel programmers are familiar with this environment.  The simplest way to install MPI is to use a precompiled version for your distribution and compiler.  

_GCC_
The recommended MPI distribution is [OpenMPI](https://www.open-mpi.org//). Most distributions provide a package.

_Intel oneAPI_
Installing the HPC Toolkit will also install IntelMPI.

_NVIDIA HPC SDK_
The NVIDIA software ships with a precompiled version of OpenMPI.

The headers and libraries for MPI _must_ match.  Using a header from one MPI and libraries from another, or using headers from a version from one compiler and libraries from a different compiler, usually results in some difficult-to-interpret bugs.  Moreover, the process manager must be compatible with the MPI used to compile the code.  Because of this, if more than one compiler and especially more than one MPI version is installed, the use of _modules_ ([environment modules](http://modules.sourceforge.net/) or [lmod](https://lmod.readthedocs.io/en/latest/) becomes particularly beneficial.  Both Intel and NVIDIA provide scripts for the environment modules package (lmod can also read these), with possibly some setup required.  If you plan to use mpi4py as well as compiled-language versions, creating a module for Anaconda would also be advisable.

#### Mac OS

_GCC_
Installing [homebrew](https://brew.sh) is the simplest way to set up MPI on a Mac.  Install the gcc package followed by the open-mpi package.

_Intel oneAPI_
Install the HPC Toolkit for IntelMPI.

_NVIDIA HPC SDK_
The NVIDIA suite is not available for Mac OS.

#### Windows

_GCC_
The simplest way to use OpenMPI on Windows is through [Cygwin](https://www.cygwin.com/).  In this case, the gcc compiler suite would first be installed, with g++ and/or gfortran added.  Then the openmpi package could also be installed through the cygwin package manager.

_Intel oneAPI_
Install the HPC Toolkit. 

_NVIDIA HPC_SDK_
Download the package when it is available.

MPI codes must generally be compiled and run through a command line on Windows.  Cygwin users can find a variety of tutorials online, for example [here](https://www.youtube.com/watch?v=ENH70zSaztM). 

The Intel oneAPI Basic Toolkit includes a customized command prompt in its folder in the Apps menu.

### Build It

In a terminal window on the frontend `rivanna.hpc.virginia.edu` run
```bash
module load gcc openmpi
```

For Python add
```bash
module load anaconda
```
This will also load the correct MPI libraries. You must have already installed mpi4py.  Activate the conda environment if appropriate.

Use mpiexec and –np **only** on the frontends!  Use for short tests only!

Compiling C 
```bash
mpicc –o mpihello mpi1.c
```

Compiling C++
```bash
mpicxx –o mpihello mpi1.cxx
```

Compiling Fortran
```bash
mpif90 –o mpihello mpi1.f90
```

### Execute it
C/C++/Fortran
```bash
mpiexec –np 4 ./mpihello
```

Python
```python
mpiexec –np 4 python mpi1.py
```

### Submit It

Write a SLURM script to run your program.  Request 1 node and 10 cores on the standard partition.  The process manager will know how many cores were requested from SLURM.
```bash
srun ./mpihello
```
Or
```bash
srun python mpihello\.py
```

## Using the Intel Compilers and MPI

Intel compilers, MPI, and math libraries (the MKL) are widely used for high-performance applications such as MPI codes, especially on Intel-architecture systems.  The appropriate MPI wrapper compilers are
```bash
#C
mpiicc -o mpihello mpi1.c
# C++
mpiicpc -o mpihello mpi1.cxx
# Fortran
mpiifort -o mpihello mpi1.f90
```
Do not use mpicc, mpicxx, or mpif90 with Intel compilers.  Those are provided by Intel but use the gcc suite and can result in conflicts.
