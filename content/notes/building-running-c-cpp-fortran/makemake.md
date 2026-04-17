---
title: makemake
date: 2026-04-14T16:42:59Z
type: docs 
weight: 600
menu: 
    building-running-c-cpp-fortran:
---

The `makemake` command is a script that will create a skeleton Makefile. Users will have to edit it to provide names, paths as needed, etc. but it is far easier than starting from scratch. 

Create a directory for the source files you wish to build and type
```bash
makemake 
```

The `makemake` script cannot distinguish files to be included in the build versus other files you may have in the directory. It will pick up all files ending in .c, .cxx, .cpp, .f, or .f90.  First edit the Makefile to remove any irrelevant file names.

At minimum you must name your program through the PROG variable. You should also specify the name of your compiler explicitly; e.g. gcc not cc.

The "phony" target `clean` is common in Makefiles.  It makes it easy to remove all compiled files and start over.  At minimum, this is necessary when compiler
options are changed.

### Makemake Skeleton: 

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

