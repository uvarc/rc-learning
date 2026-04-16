---
title: "Build Tools: Make"
date: 2026-04-14T16:42:59Z
type: docs 
weight: 500
menu: 
    building-running-c-cpp-fortran:
---

Building a program with more than one or two files can become very difficult to manage.  Various build tools have been developed. 

## Make

The oldest and still most common is `make`. Even many newer tools like `cmake` generate a `makefile`, at least on Unix. 

Make is a tool to manage builds. It has a rigid and peculiar syntax.  It will look for a file named `makefile` first, followed by `Makefile` (on case-sensitive systems).  The makefile defines one or more *targets*. The target is the product of one or more *rules*. 

The target is defined with a colon following its name. If there are *dependencies* those follow the colon.  Dependencies are other files that are required to create the current target.

### Targets and Rules

**Example**

```no-highlight
myexec: main.o module.o 
<tab>gfortran -o myexec main.o module.o
```
The tab is *required* in the rule. Don't ask why. The angle brackets are to indicate the character and are not typed.  

Macros (automatic targets) for rules:
- `$@` represents the file name of the current target
- `$<` represents the name of the first prerequisite

### Variables, Comments, and Continuations

We can define variables in makefiles 
```no-highlight
F90=gfortran
CC=gcc
CXX=icpc 
```
We then refer to them as `$(F90)`, `$(CC)`, etc. 

Common variables: F90, CC, CXX, FFLAGS, F90FLAGS, CFLAGS, CXXFLAGS, CPPFLAGS (for the preprocessor), LDFLAGS.

Comments may be inserted into a Makefile.  Anything from a `#` onward is ignored, unless it is a backslash `\`.  

The backslash is the line-continuation marker.  Be sure it is the _last_ character on the line.  If it appears as the last character in a comment line, it allows the comment to be extended over multiple lines.

### Suffix Rules

If all files with a given suffix (.c, .cxx, .f90, etc.) are to be compiled the same way, we can write a *suffix rule* to handle them. 
This uses a *phony target* called .SUFFIXES.
The rule must begin with a tab as usual. 

```
.SUFFIXES: .f90 .o
<tab>$(F90) $(F90FLAGS) -c $< 

.SUFFIXES: .cxx .cpp .o 
<tab>$(CXX) -c $(CXXFLAGS) -c <$
```

