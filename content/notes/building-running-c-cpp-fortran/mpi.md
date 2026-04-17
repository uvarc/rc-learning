---
title: Distributed Memory Parallel Programs
date: 2026-04-14T16:42:59Z
type: docs 
weight: 900
menu: 
    building-running-c-cpp-fortran:
---

Programs using distributed-memory parallelism can run on multiple nodes. They consist of independent processes that communicate through a library, usually the _Message Passing Interface_  MPI.

## Building and Running MPI Programs

MPI is an _external_ library. It must be built for the compiler with which it will be used.  We provide compiled versions of MPI for gcc and NVHPC.  For Intel we recommend using the vendor's IntelMPI.

OpenMPI with gcc and NVHPC

```
module load gcc 
```
Or

```
module load nvhpc
```

Follow this with
```
module load openmpi
```

Compile and link your code with the wrappers

-  `mpicc` for C
-  `mpicxx` for C++
-  `mpif90` for Fortran

The wrappers insert the appropriate header/module paths and library paths, and also the correct libraries to link. You do not add them explicitly.

IntelMPI with Intel
```
module load intel 
```
Use the wrappers 

-  `mpiicx` for C
-  `mpiicpx` for C++
-  `mpiifx` for Fortran

## Running MPI Programs 

MPI programs must be run under the control of an executor program. Usually this is called `mpirun` or `mpiexec`, but when submitting to SLURM we must use `mpirun` for OpenMPI and `srun` for IntelMPI.  The executor must be told how many processes to start, and if on more than one host the list of host IDs must be provided. On a local system this is controlled by command-line options to `mpirun`, e.g.
```bash
mpirun -np 8 ./mympicode
```

However, on Slurm the provided MPI executors obtain the number of processes and the hostlist directly from the job assignments. Do not specify them on the command line. 

```bash
#Intel
srun ./mympicode 
#OpenMPI
mpirun ./mympicode
```

To request resources through Slurm for an MPI program, use the following directives
```bash
#SBATCH -N NN
#SBATCH --ntasks-per-node=NT
```
where `NN` is the number of nodes and `NT` is the number of tasks (processes) to run on each node.  The total number of processes will be `NN*NT`.

**Exercise**

Copy the file mpihello.c or mpihello.f90 from /share/resources/tutorials/compilers.  C++ programmers please note: the C++ bindings are deprecated in MPI, we just use the C routines.

Load the appropriate module for the compiler you are using.

Build the code with the appropriate wrapper. 

You can run a quick test on the frontend with 4 cores. You do not need a hostfile if all processes are on one node.

Write a SLURM script using srun to run the program. Submit to the parallel partition for testing, using at least two nodes.

