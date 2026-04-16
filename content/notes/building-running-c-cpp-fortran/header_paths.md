---
title: Library and Header Paths on HPC
date: 2026-04-14T16:42:59Z
type: docs 
weight: 400
menu: 
    building-running-c-cpp-fortran:
---

Many codes need headers (C/C++) or modules (Fortran) for procedure prototypes, etc.  This is especially common if it is using an external library.
As was true for libraries, there is a system default search path for these "include files."

The compiler flag is `-I/path/to/includes`

The path to Fortran module files is also specified by the `-I` flag.

The lmod modules system we use is hierarchical. You must first load your choice of compiler. From there you can type 

```bash
module avail
```

The modules at the top will be for libraries built with that compiler. 
You will generally need to provide paths for these libraries by `-L` and `-I` 
flags. You can find the paths with

```bash
printenv | grep ROOT 
```

Most recent modules have an environment variable `NAME_ROOT`, e.g.
```no-highlight
HDF5_ROOT
```

**Example**

A number of scientific and engineering codes need the input/output libraries 
HDF5 or NetCDF. We will use NetCDF for this example. 

```bash
module load gcc
module load netcdf

printenv | grep ROOT 
```

The following should be typed on a single line:

```bash
gfortran -o myexec -L${NETCDF_ROOT}/lib -I${NETCDF_ROOT}/include mycode.f90 -lnetcdff -lnetcdf 
```

**Exercises**

Exercise 1. Multiple files (no C example here)

Copy from `/share/resources/tutorials/compilers` your choice of files
- Copy `main.cxx` or `main.f90`
- Copy `sub1.cxx` or `sub1.f90` 
- Copy `sub2.cxx` or `sub2.f90` 
- C++ copy `sub1.h` and `sub2.h` as well

Use these three files to create an executable. 

Exercise 2. External Libraries 

- Load the netcdf module for your choice of compiler suite as indicated above.
- Copy `simple_xy_wr.c` or `simple_xy_wr.cpp` or `simple_xy_wr.f90` (note the C++ suffix is `cpp` here).

- Compile and link the examples. You will need to add
    `-I${NETCDF_ROOT}/include` and `-L${NETCDF_ROOT}/lib` to your link command. For C use `-lnetcdf` for the library. For Fortran use `-lnetcdff -lnetcdf` in that order. For C++ use `-lnetcdf_c++ -lnetcdf` in that order. 

