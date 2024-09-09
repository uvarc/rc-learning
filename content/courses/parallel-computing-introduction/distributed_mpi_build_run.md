---
title: "Building and Running MPI Programs"
toc: true
type: docs
weight: 29
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

We are now ready to write our first MPI program.  

## A Hello World example.

Download the appropriate code below for your choice of language.

#### C++

Each MPI program must include the `mpi.h` header file. If the MPI distribution was installed correctly, the `mpicc` or `mpicxx` or equivalent wrapper will know the appropriate path for the header and will also link to the correct library.

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi1.cxx" lang="c++" >}}
{{< /spoiler >}}

#### Fortran

All new Fortran programs should use the `mpi` module provided by the MPI software. if the MPI distribution was installed correctly, the `mpif90` or equivalent will find the module and link to the correct library.

Any recent MPI will also provide an `mpi_f08` module.  Its use is recommended, but we will wait till [later](courses/paralll-incomputing-introduction/distributed_mpi_nonblocking_exchange) to introduce it.

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi1.f90" lang="fortran" >}}
{{< /spoiler >}}

#### Python

The `mpi4py` package consists of several objects.  Many codes will need only to import the `MPI` object.

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi1.py" lang="python" >}}
{{< /spoiler >}}

### Build It

If using an HPC system log in to the appropriate frontend, such as `login.hpc.virginia.edu`.  If the system uses a software module system, run
```bash
module load gcc openmpi
```

For Python add
```bash
module load <python distribution>
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

For HPC users, rite a Slurm script to run your program.  Request 1 node and 10 cores on the standard partition.  The process manager will know how many cores were requested from Slurm.
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
