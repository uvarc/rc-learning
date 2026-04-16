---
title: Compiler Suites
date: 2026-04-14T16:42:59Z
type: docs 
weight: 200
menu: 
    building-running-c-cpp-fortran:
---

Compilers are usually run through either an Integrated Development Environment, such as VSCOde, or through the command line in a shell. They are usually shipped by their vendor in _suites_ which consist of multiple programming languages sharing a common backend. 

## Lmod

Many HPC sites, including UVA's, provide multiple compiler suites and compiler versions, which we manage through `lmod` _modules_.  These should not be confused with modules in programming languages such as Fortran or Python; in this context the module is a script that sets up the environment for a specified software package.

For compilers, loading a module sets some necessary variables so that the correct compiler(s), linkers, and runtime libraries are in scope in the user's environment.  

Modules are loaded with the `load` command. 

```bash
module load gcc
```
sets up the default version of the GCC suite.

Modules set important environment variables such as `LOAD_LIBRARY_PATH` variable. Modules will set this variable appropriately, but since it must have the correct value, the modules used to build must also be loaded for running.

## Gnu Compiler Collection (gcc)

The GNU Compiler Collection includes front ends for C, C++, and Fortran, as well as libraries for these languages (libc, libstdc++, libgfortran). 

-  gcc is the C compiler 
-  g++ is the C++ compiler 
-  gfortran is the Fortran compiler 

To see a list of available versions, type
```bash
module spider gcc 
```
This will result in output similar to (versions will vary over time):

```no-highlight
---------------------------------------------------------------------------
  gcc:
----------------------------------------------------------------------------
    Description:
      The GNU Compiler Collection includes front ends for C, C++,
      Objective-C, Fortran, Java, and Ada, as well as libraries for these
      languages (libstdc++, libgcj,...).

     Versions:
        gcc/system
        gcc/7.1.0
        gcc/9.2.0
     Other possible modules matches:
        gcccuda

```
## Intel

Intel compilers frequently produce the fastest binaries for Intel architectures and are usually recommended for users who want the best performance.

- icx is the C compiler
- icpx is the C++ compiler 
- ifx is the Fortran compiler 

```
module spider intel
```

NVIDIA HPC SDK

We also offer the NVIDIA HPC SDK (software development kit) compilers.  This suite is particularly strong at programming for general-purpose GPUs, mainly of NVIDIA architecture. They provide tools such as OpenACC and OpenMP for programming for GPGPUs, but also support interfaces to CUDA through the higher-level languages, in particular C++ and Fortran.

-  nvcc 
-  nvc++
-  nvfortran 

```bash
module spider nvhpc
```
