---
title: Shared Memory Parallel Codes
date: 2026-04-14T16:42:59Z
type: docs 
weight: 800
menu: 
    building-running-c-cpp-fortran:
---

So far we have assumed that the code to be built is _serial_, i.e it runs on a single core.  However, many programs that are intended to run on HPC systems make use of some form of parallelism.

Shared-memory parallelism can be used on individual workstations, or on a single node of an HPC system, as long as multiple cores are present on the device.  Since nearly all modern computing devices are multicore, shared-memory parallelism is widely used in many domains.  Subprocesses called _threads_ are started by a root process. Each thread (including the root) is run on its own core.

On a multinode HPC system, programs using shared-memory parallelism must be restricted to run on a single node. This is usually handled by a resource manager such as Slurm.  The directive for Slurm is
```bash
#SBATCH --cpus-per-task=N
```
or
```bash
#SBATCH -c N
```
where N represents the number of cores requested. This directive ensures that all cores assigned to the job are on the same node.

**OpenMP**

OpenMP is one of the most popular libraries for this type of application.  OpenMP is built in to the compiler, so whether it is supported and which version is implemented is determined by the compiler vendor. It is invoked via the compiler and linker flags. 

- gcc 
      `-fopenmp`

- Intel
      `-qopenmp`

- NVHPC
      `-mp`

**Exercise**

Copy omphello.c or omphello.f90 from /share/resources/compilers

Using your choice of compiler, build an executable

- gcc --fopenmp omphello.c
- icc --qopenmp omphello.c
- pgcc --mp omphello.c

Or

- gfortran --fopenmp omphello.f90
- ifort --qopenmp omphello.f90
- pgfortran --mp omphello.c

Add a `-o <name>` flag to your build step if you want a name other than a.out.

Generally the default for OpenMP is to detect the cores and create one thread per core in parallel regions. This is not always desirable for several reasons. We can specify the number of threads for parallel regions with an environment variable.  For OpenMP we use the OMP_NUM_THREADS environment variable. 

Run your executable with 4 threads:

```bash
export OMP_NUM_THREADS=4
./a.out
```

On shared, resource-managed systems like Rivanna, The thread number must equal the number of cores requested.  We can automate this with a Slurm built-in environment variable.  In your SLURM script set

```bash
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}
```
