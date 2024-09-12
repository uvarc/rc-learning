---
title: Building and Running C/C++/Fortran codes on HPC Systems
draft: false
type: article
toc: true
date: "2023-05-01T00:00:00Z"
weight: 4000

---

## What is a Compiler

A _compiler_ is a program that converts human-written _source code_ directly into a standalone program called an _executable_ (or binary).  This is in contrast to an _interpreter_, which 
is a program that executes source code, often called a _script_ in this case, line by line.  

Compilers go through a multi-stage process to convert source code to an executable. The result is machine language and cannot be read by (most) humans.
Binaries/executables are specific to a platform, a combination of machine architecture and operating system. You cannot run a Windows binary on a Linux system, and vice versa.

When running an interpreter, the executable is the interpreter itself. Your script cannot be run directly.  Some scripts can invoke their own interpreters and run standalone, but they are not themselves binaries.

## Building an Executable

The compiler first produces an *object file* for each *source file*. In Unix these end in .o 

Object files are binary (machine language) but cannot be executed. They must be *linked* into an executable.

Libraries are special archives of compiled code that can be invoked through their application programming interface, or _API_.  Like the object files, they must be linked into an executable in order to be utilized.  

The program that generates the executable from object files and any external libraries is the _linker_ (also called a loader) is nearly always invoked through the compiler, not separately. The linker joins all object files as specified, and if run through the appropriate compiler it also links the compiler\'s *runtime libraries* for the source language. These are libraries used to carry out procedures intrinsic to the language. This happens automatically if the compiler matches the language of the main program; it is not necessary to add the runtime libraries explicitly.  However, in mixed-language programming it may be necessary to add them to the linking instructions.

## Compilers on Rivanna

Rivanna provides multiple compilers and compiler versions, which we manage through modules. 

### Gnu Compiler Collection (gcc) 

The GNU Compiler Collection includes front ends for C, C++, and Fortran, as well as libraries for these languages (libc, libstdc++, libgfortran). 

-  gcc is the C compiler 
-  g++ is the C++ compiler 
-  gfortran is the Fortran compiler 

To see a list of available versions, type
```
module spider gcc 
```
This will result in output similar to (versions will vary over time):

```
---------------------------------------------------------------------------
  gcc:
----------------------------------------------------------------------------
    Description:
      The GNU Compiler Collection includes front ends for C, C++,
      Objective-C, Fortran, Java, and Ada, as well as libraries for these
      languages (libstdc++, libgcj,...).

     Versions:
        gcc/system
        gcc/6.5.0
        gcc/7.1.0
        gcc/8.3.0
        gcc/9.2.0
     Other possible modules matches:
        gcccuda

```

### Intel

Intel compilers frequently produce the fastest binaries for Intel architectures and are usually recommended for users who want the best performance.

- icc is the C compiler
- icpc is the C++ compiler 
- ifort is the Fortran compiler 

```
module spider intel
```

Special note for Fortran users: When using the Intel compiler, nearly all Fortran codes must add a flag `-heap-arrays` to the compile line, or your executable is likely to end with a segmentation violation.

### NVIDIA HPC SDK

We also offer the NVIDIA HPC SDK (software development kit) compilers.  This suite is particularly strong at programming for general-purpose GPUs, mainly of NVIDIA architecture. They provide tools such as OpenACC and OpenMP for programming for GPGPUs, but also support interfaces to CUDA through the higher-level languages, in particular C++ and Fortran.

-  nvcc 
-  nvc++
-  nvfortran 

```bash
module spider nvhpc
```

## Compiling a Single-File Program

Suppose your program is short and can be contained within a single file. 
If not told otherwise a compiler will attempt to compile and link the source file(s) it is instructed to compile in one step.  With only one file, no separate invocation of the linker is required.

The default name for the executable is `a.out`.
The option `-o` is used to name the binary something else.

Examples for each compiler: 

- Build a C program

    - gcc -o myprog mycode.c
    - icc -o myprog mycode.c 
    - pgcc -o myprog mycode.c 

- Build a C++ program

    - g++ -o myprog mycode.cxx
    - icpc --o myprog mycode.cxx
    - pgc++ -o myprog mycode.cxx

-   Build a Fortran program

    - gfortran --o myprog mycode.f90
    - ifort --o myprog mycode.f90 
    - pgfortran --o myprog mycode.f90 


**Exercise** 

On Rivanna, load a compiler module (your choice).  Copy one of the following 
files from /share/resources/tutorials/compilers to your home directory, or to 
a directory you create for the examples. 

- mycode.c
- mycode.cxx
- mycode.f90 

Use the appropriate compiler to build your executable. Choose any name you wish for the executable.

## Compiling and Linking Multiple Files 

Most programs consist of multiple files. 
For Unix compilers the `-c` option suppresses linking. The compiler must then 
be run again to build the executable from the object files. 

```
g++ -c mymain.cxx
g++ -c mysub1.cxx
g++ -c mysub2.cxx 
g++ -o myprog mymain.o mysub1.o mysub2.o 
```

These compiler flags are similar for all three compiler suites.
```
ifort -c mymain.f90 
ifort -c mysub1.f90 
ifort -c mysub2.f90
ifort -o myprog mymain.o mysub1.o mysub2.o
```

## Linkers and Libraries

When the executable is created any external libraries must also be linked.
The compiler will search a standard path for libraries. On Unix this is typically /usr/lib, /usr/lib64, /usr/local/lib, /lib

There are two kinds of library, _static_ and _dynamic_. If the library name ends
in `.a` it is a static library; in this case, the machine code is bundled into 
the executable at compile time.  If then name ends in `.so` it is a dynamic 
library; the machine code is loaded at runtime.  For dynamic libraries, the
compiler must know the path to the library at compile time, and the executable
must be able to find the library at runtime.

If you must use paths other than the defaults, you must specify those paths to the compiler. 
`-L` followed by a path is the option for Unix compilers; then the library must be named `libfoo.a` or `libfoo.so`.  Another option provides the shortened name of the library; it is referenced as `-lfoo` in either case.  

Example

```
g++ -o mycode -L/usr/lib64/foolib mymain.o mysub1.o mysub2.o -lfoo 
```
As a general rule, libraries must be compiled by the same compiler that you use for your program.

For most Unix compilers, *order matters*! The object file containing the code being called must be linked after the object file that uses it.  Libraries come last, and must also be in the correct order.

## Headers and Modules

Many codes need headers (C/C++) or modules (Fortran) for procedure prototypes, etc.  This is especially common if it is using an external library.
As was true for libraries, there is a system default search path for these "include files."

The compiler flag is `-I/path/to/includes`

The path to Fortran module files is also specified by the `-I` flag.

### Library and Header Paths on Rivanna 

Many external libraries on Rivanna are accessed through modules.
The modules system on Rivanna is hierarchical. You must first load your choice of compiler. From there you can type 

```
module avail
```

The modules at the top will be for libraries built with that compiler. 
You will generally need to provide paths for these libraries by `-L` and `-I` 
flags. You can find the paths with

```
printenv | grep ROOT 
```

Most recent modules have an environment variable `NAME_ROOT`, e.g.
```
HDF5_ROOT
```

Example

A number of scientific and engineering codes need the input/output libraries 
HDF5 or NetCDF. We will use NetCDF for this example. 

```
module load intel
module load netcdf

printenv | grep ROOT 
```

The following should be typed on a single line:

```
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

## Running Your Executable

On a Unix system, an executable must have the right permission. Unless something goes very wrong, the compiler/linker will set the "execute bit" for your executable; you should not need to do that yourself.

In your directory that contains at least one executable run

```
ls -l
```

Make sure you are in the same directory as the executable you want to run. Type

```
./myprog 
```
Use the name you assigned your executable. `./` means current directory and is necessary because that is not in your default path.

## Build Tools

This can all become very difficult to manage when codes have a lot of files. 
Various build tools have been developed. 

### Make

The oldest and still most common is make. Even many newer tools like cmake generate a makefile, at least on Unix. 

Make is a tool to manage builds. It has a rigid and peculiar syntax. 
It will look for a file named `makefile` first, followed by `Makefile` (on case-sensitive systems).
The makefile defines one or more *targets*. The target is the product of one or more *rules*. 

The target is defined with a colon following its name. If there are *dependencies* those follow the colon.
Dependencies are other files that are required to create the current target.

#### Targets and Rules

Example

```
myexec: main.o module.o 
<tab>gfortran -o myexec main.o module.o
```
The tab is *required* in the rule. Don't ask why. The angle brackets are to indicate the character and are not typed.  

Macros (automatic targets) for rules:
- `$@` represents the file name of the current target
- `$<` represents the name of the first prerequisite

Variables, Comments, and Continuations: 

We can define variables in makefiles 
```
F90=gfortran
CC=gcc
CXX=icpc 
```
We then refer to them as `$(F90)`, `$(CC)`, etc. 

Common variables: F90, CC, CXX, FFLAGS, F90FLAGS, CFLAGS, CXXFLAGS, CPPFLAGS (for the preprocessor), LDFLAGS.

Comments may be inserted into a Makefile.  Anything from a `#` onward is ignored, unless it is a backslash `\`.  

The backslash is the line-continuation marker.  Be sure it is the _last_ character on the line.  If it appears as the last character in a comment line, it allows the comment to be 
extended over multiple lines.

Suffix Rules:
If all files with a given suffix (.c, .cxx, .f90, etc.) are to be compiled the same way, we can write a *suffix rule* to handle them. 
This uses a *phony target* called .SUFFIXES.
The rule must begin with a tab as usual. 

```
.SUFFIXES: .f90 .o
<tab>$(F90) $(F90FLAGS) -c $< 

.SUFFIXES: .cxx .cpp .o 
<tab>$(CXX) -c $(CXXFLAGS) -c <$
```

Pattern Rules:
This is an extension by Gnu make (gmake), but nearly every make, especially on Linux, is gmake now.  They are similar to suffix rules. 

Gmake contains built-in pattern rules so it can handle common cases if you do not write your own rules.  For example, to compile a C code it will by default use
```
%.o : %.c
        $(CC) -c $(CFLAGS) $(CPPFLAGS) $< -o $@
```

A pattern rule that is particularly useful for Fortran 90+:
```
%.mod: %.o 
```

This can be helpful because make's default pattern rule for `.mod` is for a programming language called Modula, but Fortran uses `.mod` for compiled module interface files.  Adding this pattern rule overrides the built-in rule for that suffix.

#### Make Options

Files with names other than `makefile` or `Makefile` can be used with the -f option
```
make -f Make.linux 
```

Make creates the first target in the file unless a specific target is specified.(Some Makefiles are written to require a target.). To generate a different target, provide its name on the command line.

```
make pw.x 
```

#### makemake

On Rivanna we provide a script that will create a skeleton Makefile. Users will have to edit it to provide names, paths as needed, etc. but it is far easier than starting from scratch. 

Create a directory for the source files you wish to build and type
```
makemake 
```

The `makemake` script cannot distinguish files to be included in the build versus other files you may have in the directory. It will pick up all files ending in .c, .cxx, .cpp, .f, or .f90.  First edit the Makefile to remove any irrelevant file names.

At minimum you must name your program through the PROG variable. You should also specify the name of your compiler explicitly; e.g. gcc not cc.

The "phony" target `clean` is common in Makefiles.  It makes it easy to remove all compiled files and start over.  At minimum, this is necessary when compiler
options are changed.

Makemake Skeleton: 

```make
PROG = 

SRCS =  code.cxx file1.cxx file2.cxx 

OBJS =  code.o file1.o file2.o

LIBS = 

CC = cc

CXX = c++

CFLAGS = -O 

CXXFLAGS = -O

FC = f77 

FFLAGS = -O 

F90 = f90 

F90FLAGS = -O

LDFLAGS = 

all: $(PROG) 

$(PROG): $(OBJS)
        $(CXX) $(LDFLAGS) -o $@ $(OBJS) $(LIBS) 

.PHONY: clean

clean: 
        rm -f $(PROG) $(OBJS) *.mod 

.SUFFIXES: $(SUFFIXES) .f .f90 .F90 .f95

.SUFFIXES: $(SUFFIXES) .c .cpp .cxx 

.f90.o .f95.o .F90.o: 
        $(F90) $(F90FLAGS) -c $< 

.f.o: 
        $(FC) $(FFLAGS) -c $< 

.c.o: 
        $(CC) $(CFLAGS) -c $< 

.cpp.o .cxx.o:
        $(CXX) $(CXXFLAGS) -c $< 

main.o: code.cxx file1.h file2.h 
```

**Exercise**

Copy or move your multi-file program into its own directory. 
Run makemake in the directory.

Edit the resulting Makefile appropriately. You may remove any lines pertaining to languages you aren't using.

Run make.

Check that your executable works. 

### Configure 

Configure is a way to generate a Makefile automatically.

We will not discuss creating configure scripts, since they can be quite complex. But much software is distributed with them.
Configure usually takes many options and they vary by package. To see them, run from its directory 

```make
./configure --help
```

Most configure scripts assume the software will be installed into /usr or /usr/local. You do not have permission to do this on Rivanna so you will nearly always need to use the `-prefix` option. 

```make
./configure -prefix=/home/mst3k/udunits
```

Configure should produce a Makefile. 
When it has completed you can run `make` as usual. This is usually followed by

```make
make install
```

Refer to the documentation for your package for more details.

**Exercise** 

Copy udunits.tar.gz from /share/resources/tutorials/compilers

Untar it (`tar xf udunits.tar.gz`). Cd into its directory. Run

```
configure -help
```

After examining the options, run it with at least the prefix option. You may wish to create a new directory for installing the package.

Build the library.

Install the library into the directory prepared for it.

### CMake

CMake is a platform-independent build system.
Unlike configure, it can be used on Windows natively. 
On Rivanna, the system cmake is very old. Newer versions are available through modules. 

```
module spider cmake 
```

The default module version should be sufficiently recent to build most cmake projects. 

```
module load cmake 
```

#### Running CMake 

CMake usually requires the creation of a separate build directory below the top-level directory of the project. 
CMake uses -D flags as the equivalents of many options to configure.
CMake caches your configuration into a file named CMakeCache.txt -- if you make changes you must remove this file or your changes will be ignored. It will be in the build directory if you created one.

#### Useful CMake Flags

You will usually need to add 

```
-DCMAKE_INSTALL_PREFIX=/path/to/install/location 
```
This is the equivalent of the `-prefix` option to `configure`.

CMake does not pay (much) attention to your paths. It has its own means of finding compilers and libraries.
You will probably need to add

```
-DCMAKE_C_COMPILER=gcc
-DCMAKE_CXX_COMPILER=g++
-DCMAKE_FC_COMPILER=gfortran 
```

Cmake Example

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

## Building and Running Parallel Codes

### Shared Memory

Programs using shared-memory parallelism must run on a single node. Threads are started by a master process. OpenMP is one popular library for this type of application.

**OpenMP**

OpenMP is a library that is built in to the compiler. It is invoked via compiler and linker flags. 

- gcc 
      `-fopenmp`

- Intel
      `-qopenmp`

- PGI
      `-mp`

Generally the default for OpenMP is to create one thread per core in parallel regions.
This is not permitted on shared, resource-managed systems like Rivanna. The thread number must equal the number of cores requested.

For OpenMP we use the OMP_NUM_THREADS environment variable. 
In a SLURM script set
```plaintext
#SBATCH -c <N> 
```
where `N` is the number of cores you wish to use on the node.  Then in your commands, before you run your code, set 
```bash
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}
```

OpenMP Example 

Copy omphello.c or omphello.f90 from /share/resources/compilers
Using your choice of compiler, build an executable 

- gcc --fopenmp omphello.c 
- icc --qopenmp omphello.c 
- pgcc --mp omphello.c

Or

- gfortran --fopenmp omphello.f90
- ifort --qopenmp omphello.f90 
- pgfortran --mp omphello.c 

Run The OpenMP Program 

Add a `-o <name>` flag to your build step if you want a name other than a.out.

```
export OMP_NUM_THREADS=4 

./a.out 
```

#### Distributed memory 

Programs using distributed-memory parallelism can run on multiple nodes. Independent processes that communicate through a library, usually MPI.

**Building and Running MPI Programs**

MPI is an _external_ library. It must be built for the compiler with which it will be used.
We provided compiled versions of MPI for GCC and PGI. 
For Intel we recommend using the vendor\'s IntelMPI.

OpenMPI with GCC and PGI

```
module load gcc 
```
Or

```
module load pgi
```

Follow this by
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
module load intelmpi
```
Use the wrappers 

-  `mpiicc` for C
-  `mpiicpc` for C++
-  `mpiifort` for Fortran

Running MPI Programs 

MPI programs must be run under the control of an executor program. Usually this is called `mpirun` or `mpiexec`, but when submitting to SLURM we must use `srun`.
The `mpirun` and `mpiexec` must be told how many processes to start, and if on more than one host the list of host IDs must be provided.
The SLURM executor `srun` obtains the number of processes and the hostlist directly from the job assignments. Do not specify them on the command line. 

```plaintext
srun mympicode 
```

On our system, mpirun/mpiexec do not communicate with SLURM. Always use srun as the executor in SLURM scripts.  You may use mpirun on the frontend with a small number of processes for very short test runs.
```plaintext
mpirun -np 4 mympicode
```

**Exercise**

Copy the file mpihello.c or mpihello.f90 from /share/resources/tutorials/compilers.
C++ programmers please note: the C++ bindings are deprecated in MPI, we just use the C routines.

Load the appropriate module for the compiler you are using.

Build the code with the appropriate wrapper. 

You can run a quick test on the frontend with 4 cores. You do not need a hostfile if all processes are on one node.

Write a SLURM script using srun to run the program. Submit to the devel partition for testing.

### Integrated Development Environments

IDEs are a graphical user interface for code development. 

Examples include Microsoft Visual Studio, XCode, and  Eclipse.

IDEs installed on Rivanna:
- Geany
- Code::Blocks
- Eclipse

IDEs require access to X11, the Unix graphical windowing system. One way to accommodate this is through FastX Web.

```
https://rivanna-desktop.hpc.virginia.edu 
```

#### Geany

Geany is a programmer\'s editor that also has some lightweight building capabilities.
Geany is easy to use and excellent for less complicated projects. It provides syntax coloring for dozens of languages.
It runs on Linux, Windows, and Mac so you can easily install it onto your personal computers. 
By default it uses gcc/g++/gfortran versions that are in the path (so load the desired gcc module _before_ loading the geany module). 
You can alter the compiler commands from the Build menu, such as for using the intel compilers.
```plaintext
Build->Set Build Commands
```
Building with Geany:

For a single-file program, Build-\>Build or the brick icon.
For a multi-file program, or more than one project, each file must be individually compiled.

```plaintext
Build->Compile
``` 
or the dropdown next to the brick.
You can use Make but must write the Makefile yourself (you can edit it with Geany). 

When complete, you can run the program with Execute if it\'s a short test.

#### Code::Blocks

Code::Blocks is a somewhat more conventional IDE for C/C++/Fortran.
The module is codeblocks (no colon)
```
module load codeblocks 
```

It is based on the notion of "projects" rather than individual files.
It will need to add "project" files to any folders you want to use for your code. It is simplest to start each project from a new folder. 
Code can be built in two modes: Debug, or Production.  Debug adds a debugging flag `-g` whereas Production adds the optimization flag `-O`.

#### Eclipse

Usage of Eclipse for C/C++/Fortran is more complicated.  Please see the [documentation](http://help.eclipse.org).  
