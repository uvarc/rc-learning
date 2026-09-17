---
title: OpenACC Basics
date: 2026-09-2T17:26:29Z
type: book 
weight: 4050
menu: 
    parallel_programming:
        parent: Accelerator Programming   
---

The syntax of OpenACC is very similar to that of OpenMP, but with `acc` in place of `omp`.

Some examples for C/C++

```c
#pragma acc parallel
#pragma acc parallel if (<conditional>)
#pragma acc parallel private (varlist)
```

OpenACC uses a generic loop rather than for/do
```
#pragma acc parallel
#pragma acc loop
```

The Fortran equivalents are

```fortran
!$acc parallel
!$acc loop
!$acc end parallel
```

OpenACC implements a reduction clause with the same syntax and operators as [OpenMP reductions](/courses/parallel-computing-introduction/shared_memory/multithread_omp_reductions]).

## Building Code

In order to build for a device like a GPU, it is necessary for the device to exist on the system, have a working driver installed, and have any needed libraries available.

### NVIDIA HPC SDK

The NVIDIA [HPC SDK](https://developer.nvidia.com/hpc-sdk) provides the most complete support for NVIDIA hardware. It ships with at least one version of CUDA, usually the latest or the latest two at the time of its release. The choice of one or multiple CUDA versions is made when the compiler suite is downloaded for installation.  Once installed, it will use the newest version of CUDA that matches the CUDA driver it finds.

OpenACC does not generally require a header or module to be included in the code. It is invoked though a compiler flag.

If using the NVIDIA HPC SDK suite, use the flag `-acc`. This flag can take options including
* -acc=gpu  Compile for a generic GPU on the system
* -acc=multicore Compile for a multicore CPU
* -acc=host Compile for serial execution on the host

If no option is specified, the default is to compile for an NVIDIA GPU.

A separate option is available to specify GPU `-gpu=<option>` for NVIDIA architectures only. This flag requires an option. If the `-gpu` flag is included, the generated code will be specific to that option and may not be portable to devices or drivers not covered by the option.


See the [documentation](https://docs.nvidia.com/hpc-sdk/index.html) for details of compiler flags.

### GCC

Use the flag `-fopenacc`.  Support for GPUs, in particular NVIDIA GPUs, is less comprehensive in GCC than in the HPC SDK.

GCC provides [documentation](https://gcc.gnu.org/onlinedocs/gcc/OpenMP-and-OpenACC-Options.html) for options related to OpenACC and OpenMP.




