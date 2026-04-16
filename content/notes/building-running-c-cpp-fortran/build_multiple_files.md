---
title: Compiling and Linking Multiple Files
date: 2026-04-14T16:42:59Z
type: docs 
weight: 300
menu: 
    building-running-c-cpp-fortran:
---

Most programs consist of multiple files.  For Unix compilers the `-c` option suppresses linking. The compiler must then be run again to build the executable from the object files. 

```bash
g++ -c mymain.cxx
g++ -c mysub1.cxx
g++ -c mysub2.cxx 
g++ -o myprog mymain.o mysub1.o mysub2.o 
```

These compiler flags are similar for all three compiler suites.
```bash
ifx -c mymain.f90 
ifx -c mysub1.f90 
ifx -c mysub2.f90
ifx -o myprog mymain.o mysub1.o mysub2.o
```

## Linkers and Libraries

When the executable is created any external libraries must also be linked.
The compiler will search a standard path for libraries. On Unix this is typically /usr/lib, /usr/lib64, /usr/local/lib, /lib

There are two kinds of library, _static_ and _dynamic_. If the library name ends
in `.a` it is a static library; in this case, the machine code is bundled into 
the executable at compile time. If the name ends in `.so` it is a shared 
library; the machine code is loaded at runtime.  For shared libraries, the
compiler must know the path to the library at compile time, and the executable
must be able to find the library at runtime. For Unix/Linux systems, shared librries are the default.

If you must use paths other than the defaults, you must specify those paths to the compiler. 
`-L` followed by a path is the option for Unix compilers; then the library must be named `libfoo.a` or `libfoo.so`.  Another option provides the shortened name of the library; it is referenced as `-lfoo` in either case.  

**Example**

```
g++ -o mycode -L/usr/lib64/foolib mymain.o mysub1.o mysub2.o -lfoo 
```
As a general rule, libraries must be compiled by the same compiler that you use for your program.  However, Intel and NVHPC compilers can usually link C (not necessarily C++) system libraries.

For most Unix compilers, *order matters*! The object file containing the code being called must be linked after the object file that uses it.  Libraries come last, and must also be in the correct order.

