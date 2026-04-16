---
title: CMake
date: 2026-04-14T16:42:59Z
type: docs 
weight: 750
menu: 
    building-running-c-cpp-fortran:
---

[CMake](https://cmake.org) is a platform-independent build system.  Unlike configure, it can be used on Windows natively.  It is popular especially for software that must be built for multiple platforms.

CMake usually requires the creation of a separate build directory below the top-level directory of the project. 
CMake uses -D flags as the equivalents of many options to configure.
CMake caches your configuration into a file named CMakeCache.txt -- if you make changes you must remove this file or your changes will be ignored. It will be in the build directory if you created one.

### Useful CMake Flags

You will often need to add 

```bash
-DCMAKE_INSTALL_PREFIX=/path/to/install/location 
```
This is the equivalent of the `-prefix` option to `configure`.

CMake does not pay (much) attention to your paths. It has its own means of finding compilers and libraries.  You may need to set some environment variables if you use a compiler loaded from an lmod module.

```bash
-DCMAKE_C_COMPILER=gcc
-DCMAKE_CXX_COMPILER=g++
-DCMAKE_FC_COMPILER=gfortran 
```

There are many versions of CMake and some CMake scripts provided by developers require a particular version or range of versions.  The system cmake may be old or not suitable. Newer versions are available through modules.

```bash
module spider cmake
```
The default module version should be sufficiently recent to build most cmake projects.

```bash
module load cmake
```

**Example**

We will use an example prepared by Thom Troy (ttroy50 on GitHub)
Copy cmake-example.tar.gz from /share/resources/tutorials/compilers

Extract the tar-file and cd to the directory it creates.

Make an installation directory.

Make a build directory. Cd into it.

Load a compiler if you haven't already, for example gcc.

Load the cmake module.

Run (type cmake command all on one line) 

```
cmake .. -DCMAKE_INSTALL_PREFIX=/home/yourid/yourdir -DCMAKE_CXX_COMPILER=g++

make
```

This simple example doesn't create an installation target, so move the binary to the intended location. 


