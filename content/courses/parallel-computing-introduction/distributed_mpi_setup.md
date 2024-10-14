---
title: "Setting Up MPI"
toc: true
type: docs
weight: 28
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

Using MPI requires access to a computer with at least one node with multiple cores.  The Message Passing Interface is a standard and there are multiple implementations of it, so a choice of distribution must be made.  Popular implementations include [MPICH](https://www.mpich.org/), [OpenMPI](https://www.open-mpi.org/), [MVAPICH2](https://mvapich.cse.ohio-state.edu/), and [IntelMPI](https://www.intel.com/content/www/us/en/developer/tools/oneapi/mpi-library.html#gs.gdkhva).  MPICH, OpenMPI, and MVAPICH2 must be built for a system, so a compiler must be chosen as well.  IntelMPI is typically used with the Intel compiler and is provided by the vendor as part of their [HPC Toolkit](https://www.intel.com/content/www/us/en/developer/tools/oneapi/hpc-toolkit.html#gs.gdkm8y).  MVAPICH2 is a version of MPICH that is specialized for high-speed [Infiniband](https://en.wikipedia.org/wiki/InfiniBand) networks on high-performance clusters, so would generally not be appropriate for installation on individual computers.

### On a Remote Cluster

Refer to the instructions from your site, for example [UVA Research Computing](https://www.rc.virginia.edu/userinfo/howtos/rivanna/mpi-howto/) for our local environment.  Nearly always, you will be required to prepare your code and run it through a _resource manager_ such as [Slurm](https://www.rc.virginia.edu/userinfo/rivanna/slurm/).  Most HPC sites use a _modules_ system, so generally you will need to load modules for an MPI version and usually the corresponding compiler.  It is important to be sure that you use a version of MPI that can communicate correctly with your resource manager.
```bash
module load gcc
module load openmpi
```
is an example setup for compiled-language users. 

For Python, the mpi4py package is most widely available. It is generally preferable, and may be required, that mpi4py be installed from the conda-forge repository.  On a cluster, mpi4py will need to link to a locally-built version of MPI that can communicate with the resource manager.  The conda-forge maintainers provide instructions for this [here](https://conda-forge.org/docs/user/tipsandtricks/#using-external-message-passing-interface-mpi-libraries). In our example, we will use openmpi.  First we must load the modules for the compiler and MPI version:

```bash
module load gcc openmpi
```
We must not install OpenMPI directly from conda-forge; rather we make use of the "hooks" they have provided.  
```bash
module list openmpi
```
In our example, the module list returns
```bash
Currently Loaded Modules Matching: openmpi
  1) openmpi/4.1.4
```
Now we check that our version of OpenMPI is available
```bash
conda search -f openmpi -c conda-forge
```
Most versions are there, so we can install the one we need
```bash
conda install -c conda-forge "openmpi=4.1.4=external_*"
```
Be sure to include the `external_*` string.  

After this completes, we can install mpi4py
```bash
conda install -c conda-forge mpi4py
```

### On a Local Computer

If you have access to a multicore computer, you can run MPI programs on it.

If using a compiled language, before you can build MPI codes you must install a compiler, and possibly some kind of IDE.  See our guides for [C++](/courses/cpp_introduction/setting_up) or [Fortran](/courses/fortran_introduction/setting_up).

For Python, on all operating systems install [mpi4py](https://mpi4py.readthedocs.io/en/stable/index.html). To install mpi4py you must have a working `mpicc` compiler.  If you use `conda` or `mamaba` from a distribution like [miniforge](https://github.com/conda-forge/miniforge), the required compiler will be installed as a dependency.  For `pip` installations you must provide your own compiler setup. 

The author of mpi4py [recommends](https://mpi4py.readthedocs.io/en/stable/install.html) using pip even with a conda environment. This command will be similar on a local system to that used for installation on a multiuser system. 
```no-highlight
python -m pip install mpi4py
```
This may avoid some issues that occasionally arise in prebuilt mpi4py packages. Be sure that an appropriate `mpicc` executable is in the path.  Alternatively, use the `conda-forge` channel (recommended in general for most scientific software).  Most of the time, if you are installing mpi4py from conda-forge, you can simply install the package.  MPICH is the default when installed as a prerequisite for conda-forge.

#### Linux

Parallel programs are most frequently used in a _high-performance computing_ environment such as the clusters we have discussed, but many multicore workstations are available that can run Linux and many parallel programmers are familiar with this environment.  The simplest way to install MPI is to use a precompiled version for your distribution and compiler.  

_GCC_
The recommended MPI distribution is [OpenMPI](https://www.open-mpi.org//). Most distributions provide a package.

_Intel oneAPI_
Installing the HPC Toolkit will also install IntelMPI.

_NVIDIA HPC SDK_
The NVIDIA software ships with a precompiled version of OpenMPI.

The headers and libraries for MPI _must_ match.  Using a header from one MPI and libraries from another, or using headers from a version from one compiler and libraries from a different compiler, usually results in some difficult-to-interpret bugs.  Moreover, the process manager must be compatible with the MPI used to compile the code.  Because of this, if more than one compiler and especially more than one MPI version is installed, the use of _modules_ ([environment modules](http://modules.sourceforge.net/) or [lmod](https://lmod.readthedocs.io/en/latest/)) becomes particularly beneficial.  Both Intel and NVIDIA provide scripts for the environment modules package (lmod can also read these), with possibly some setup required.  If you plan to use mpi4py as well as compiled-language versions, creating a module for your Python distribution would also be advisable. Installation of a module system on an individual Linux system is straightforward for an administrator with some experience.

#### Mac OS

_GCC_
Installing [homebrew](https://brew.sh) is the simplest way to set up MPI on a Mac.  Install the gcc package followed by the open-mpi package.

_Intel oneAPI_
Install the HPC Toolkit for IntelMPI.

_NVIDIA HPC SDK_
The NVIDIA suite is not available for Mac OS.

#### Windows

_GCC_
The easiest way to use OpenMPI on Windows is through [Cygwin](https://www.cygwin.com/).  In this case, the gcc compiler suite would first be installed, with g++ and/or gfortran added.  Then the openmpi package could also be installed through the cygwin package manager.

_Intel oneAPI_
Install the HPC Toolkit. 

_NVIDIA HPC_SDK_
Download the package when it is available.

MPI codes must generally be compiled and run through a command line on Windows.  Cygwin users can find a variety of tutorials online, for example [here](https://www.youtube.com/watch?v=ENH70zSaztM). 

The Intel oneAPI Basic Toolkit includes a customized command prompt in its folder in the Apps menu.
