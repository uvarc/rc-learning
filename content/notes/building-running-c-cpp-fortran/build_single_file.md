---
title: Compiling a Single-File Program
date: 2026-04-14T16:42:59Z
type: docs 
weight: 250
menu: 
    building-running-c-cpp-fortran:
---

Suppose your program is short and can be contained within a single file. 
If not told otherwise a compiler will attempt to compile and link the source file(s) it is instructed to compile in one step.  With only one file, no separate invocation of the linker is required.

The default name for the executable is `a.out`.
The option `-o` is used to name the binary something else.

Examples for each compiler: 

- Build a C program

    - gcc -o myprog mycode.c
    - icx -o myprog mycode.c 
    - nvcc -o myprog mycode.c 

- Build a C++ program

    - g++ -o myprog mycode.cxx
    - icpx --o myprog mycode.cxx
    - nvc++ -o myprog mycode.cxx

-   Build a Fortran program

    - gfortran --o myprog mycode.f90
    - ifx --o myprog mycode.f90 
    - nvfortran --o myprog mycode.f90 


**Exercise** 

Load a compiler module (your choice).  Copy one of the following 
files from /share/resources/tutorials/compilers to your home directory, or to 
a directory you create for the examples. 

- mycode.c
- mycode.cxx
- mycode.f90 

Use the appropriate compiler to build your executable. Choose any name you wish for the executable.


## File Permissions

On a Unix system, an executable must have the right permission. Unless something goes very wrong, the compiler/linker will set the "execute bit" for your executable; you should not need to do that yourself.

In your directory that contains at least one executable run

```bash
ls -l
```

Make sure you are in the same directory as the executable you want to run. Type

```bash
./myprog
```
Use the name you assigned your executable. `./` means current directory and is necessary because that is not in your default path.

