---
title: "Building an Executable"
toc: true
type: docs
weight: 15

menu:
    fortran_introduction:
        name: Building an Executable
        weight: 15

---

# Building an Executable

The compiler first produces an _object file_ for each _source file_ .  In Unix these end in.o

Object files are binary (machine language) but cannot be executed.  They must be linked into an executable.

If not told otherwise a compiler will attempt to compile and link the source file(s) it is instructed to compile.

For Unix compilers the-coption suppresses linking.  The compiler must then be run again to build the executable from the object files.

The option-ois used to name the binary something other thana.out

# Linkers and Libraries

When the executable is created any external libraries must also be linked.

The compiler will search a standard path for libraries.  On Unix this is typically/usr/lib, /usr/lib64, /usr/local/lib, /lib

If you have others you must give the compiler the path.-Lfollowed by a path works, then the libraries must be namedlibfoo.aorlibfoo.soand it is referenced-lfoo

Example:

gfortran–omycode–L/usr/lib64/foolibmymain.omysub.o-lfoo

# Make

makeis a tool to manage builds, especially with multiple files.

It has a rigid and peculiar syntax.

It will look for amakefilefirst, followed byMakefile(on case-sensitive systems).

Themakefiledefines one or more _targets_ .  The target is the product of one or more _rules_ .

The target is defined with a colon following its name.  If there are _dependencies_ those follow the colon.

Dependencies are other files that are required to create the current target.

# Targets and Rules

Example:

myexec:main.omodule.o

<tab>gfortran-omyexecmain.omodule.o

The tab is _required_ in the rule.  Don’t ask why.

Macros (automatic targets) for rules:

$@the file name of the current target

$<the name of the first prerequisite

# Variables and Comments

We can define variables inmakefiles

F90=gfortran

CXX=g++

We then refer to them as$(F90),$(CXX), etc.

Common variables:F90, CC, CXX, FFLAGS, F90FLAGS, CFLAGS, CXXFLAGS, CPPFLAGS(for the preprocessor), LDFLAGS

# Suffix Rules

* If all .f90 (or .cc or whatever) files are to be compiled the same way, we can write a _suffix rule_ to handle them.
* It uses a _phony target_ called .SUFFIXES.
* .SUFFIXES: .f90 .o
  * $(F90) -c $(F90FLAGS) –c $<

# Pattern Rules

* An extension by Gnu make (gmake), but nearly everymakeisgmakenow.
* Similar to suffix rules.
* Useful for Fortran 90+:
* %.mod: %.o
* Pattern for creating the .o:
* %.o: %.f90
  * $(F90) $(F90FLAGS) -c $<

# Example

PROG =bmidata

SRCS =  bmi_calculator.f90 bmi_data.f90 csv_file.f90 prec.f90 stats.f90

OBJS =bmi_calculator.obmi_data.ocsv_file.oprec.ostats.o

LIBS =

F90 =gfortran

#F90FLAGS=-O

F90FLAGS = -g -C

LDFLAGS =

all: $(PROG)

$(PROG): $(OBJS)

$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

.PHONY: clean

clean:

rm-f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .f90 .F90 .f95

.f90.o .f95.o .F90.o:

$(F90) $(F90FLAGS) -c $<

stats.o:prec.o

bmi_calculator.o:csv_file.oprec.o

bmi_data.o:bmi_calculator.ocsv_file.oprec.ostats.o

