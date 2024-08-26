---
date : "2021-06-23T00:00:00-05:00"
title: "Linkers and Libraries"
toc: true
type: book
weight: 13

---

## Linkers and Libraries

When the executable is created any external libraries must also be linked.
The compiler will search a standard path for libraries.  This path is dependent on both the operating system and the compiler, but on Unix this is typically `/usr/lib`, `/usr/lib64`, `/usr/local/lib`, `/lib`.
If you need libraries in other locations, you must give the compiler the path. `-L` followed by a path works, then each library must be named with the pattern `libfoo.a` or `libfoo.so` and be referenced `-lfoo`.  
Subdirectories are not included in the library search paths so a `-L` flag is required even when the path begins with a standard location.

Example:
```
g++ –o mycode –L/usr/lib64/foo/lib mymain.o mysub.o -lfoo
```

A library ending in `.a` is _static_.  Its machine-language code will be physically incorporated into the executable.  If the library ends in `.so` it is _dynamic_.  It will be invoked by the executable at runtime.

MacOS is similar to Unix but generally uses different paths for the system libraries.

Working with libraries on Windows is somewhat more complex.  Dynamic libraries are called DLLs (dynamically linked libraries) and end in `.dll`, but generally code links an "import library" that ends in `.lib`.  Although the underlying DLL is supposed to be universal, these import library files are compiler dependent.  Microsoft Visual Studio will automatically link to the .lib but MingGW uses a more Unix-like format by default and expects a `.so` suffix.  Libraries provided by Windows will be in MSVC (Microsoft Visual C++) format so translation may be required.  MinGW-64 provides the `gendef` tool that can create a definition file from a DLL, which can then be used to produce a MinGW import library.  See the MinGW-64 [documentation](https://sourceforge.net/p/mingw-w64/wiki2/gendef/) for specifics.  The MinGW-64 [FAQ](https://sourceforge.net/p/mingw-w64/wiki2/FAQ/) is also helpful.

## Headers and the C Preprocessor

Most C and C++ libraries require _include files_, also called _header_ files.
Indeed, some libraries consist only of headers. 
The headers must be made available to your source code, in addition to the bodies of the libraries being linked into the binary.  
The headers are brought in through the C Preprocessor (CPP).  CPP directives begin with a hash mark `#` in the first column.  

The `#include` directive physically copies the referenced file at the specified point into an intermediate text file used by the compiler.  The processor will have a built-in search path for headers and those headers that are in that path are referenced with angle brackets.
```c++
#include <iostream>
#include <string>
```
Any `.h` or `.hpp` suffix is usually omitted.  C libraries are prefixed with `c` in addition to dropping the suffix.
```
#include <cstdio>
#include <cmath>
```
These files are `stdio.h` and `math.h` respectively in C programs.

For local variables, quotation marks tell the preprocessor to look in the current directory/folder relative to the file that includes the header.  If not found there, most systems will move on to the system folders.
```c++
#include "myheader.h"
```
Sometimes a longer path is used.  The processor will start looking in its search path (system folders or current directory as appropriate).
```c++
#include <boost/atomic/atomic.hpp>
#include "subs/mysubs.h"
```
Paths to headers that are not in the default search path nor in the current directory 
can be specified at compile time with the `-I` flag.

Example:
```bash
g++ –c –I/usr/lib64/foo/include mymain.cxx
```

Many C++ "community" codes use the [CMake](https://cmake.org) build system.  CMake generates a Makefile but uses its own search mechanism for headers and libraries.  See for example this [tutorial](https://gitlab.kitware.com/cmake/community/-/wikis/doc/tutorials/How-To-Find-Libraries).

## Compiler Libraries

If the compiler is used to invoke the linker, as we have done for all our examples, it will automatically link several libraries, the most important of which for our purposes are the _runtime libraries_.  An executable must be able to start itself, request resources from the operating system, assign values to memory, and perform many other functions that can only be carried out when the executable is run.  The runtime libraries enable it to do this.  As long as all the program files are written in the same language and the corresponding compiler is used for linking, this will be invisible to the programmer.  Sometimes, however, we must link runtime libraries explicitly, such as when we are mixing languages (a main program in Fortran and some low-level routines in C, or a main program in C++ with subroutines from Fortran, for instance).  

All C++ compilers also provide a set of _standard libraries_ that implement many of the features of the language, such as the data structures defined in the _Standard Template Library_ or **STL**. These libraries generally require headers to define their functions.  We have already been using the `iostream` library but there are many others.

```c++
#include <iostream>
#include <stringstream>
#include <vector>
```
Most C++ compilers are also C compilers, and vice versa, and use the C runtime library, which is fairly minimal.  For example, it is possible to use gcc to compile C++ code by linking the standard library.  In the following example the program used `sqrt` so it was necessary to link the math library libm.so explicitly; C++ does not require this.
```no-highlight
gcc vardecls.cxx -lstdc++ -lm
```
The Gnu C++ standard libraries are in libstdc++.so on Unix.  It is obviously much simpler to type
```no-highlight
g++ vardecls.cxx
```
## Compiling and Linking Multiple Files with an IDE

Our discussion of building your code has assumed the use of a command line on Unix.  An IDE can simplify the process even on that platform.
We will use Geany for our example; more sophisticated IDEs have more capabilities, but Geany illustrates the basic functions.
Using Microsoft tools such as Visual Studio or VSCode on Windows may be desirable if you will need to link to Microsoft or other vendor-provided libraries.

We have three files in our project, [example.cxx](/courses/cpp-introduction/compiler_example/example.cxx), [adder.cxx](/courses/cpp-introduction/compiler_example/adder.cxx) and [adder.h](/courses/cpp-introduction/compiler_example/adder.h).  The main program is `example.cxx`.  It needs `adder.cxx` to create the executable.  We must open the two files in Geany.  Then we must compile (not build) each one separately.  Once we have successfully compiled both files, we open a terminal window (cmd.exe on Windows).  We navigate to the folder where the files are located and type 
```
g++ -o example example.o adder.o
```
Notice that we name the executable the same as the main program, minus the file extension.  This follows the Geany convention for the executable.  It is not a requirement but if Geany is to execute it, that is the name for which it will look.

You can run the executable either from the command line (`./example` may be required for Linux) or through the Geany execute menu or gears icon.  
If Geany is to run a multi-file executable then the main program file must be selected as the current file as well as match the name of the executable.

{{< figure src="/courses/cpp-introduction/img/Geany5.png" width=500px caption="Executing the example program" >}}

The file ending in `.h` is called a _header file_. The best practice in C++ is to separate the _interface_ into a header file and the _implementation_ into the source file. We will discuss this, as well as the preprocessor commands, when we cover functions.

This process becomes increasingly cumbersome as projects grow in number and complexity of files.  The most common way to manage projects is through the [make](courses/cpp-introduction/make.md) utility, which we will examine next.
