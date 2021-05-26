---
title: "Compilers and IDEs"
toc: true
type: book
weight: 11

menu:
    fortran_introduction:
        parent: Building and Running Your Programs
        weight: 11

---

The "traditional" development environment for compiled languages was a text editor and a command line.  Many programmers continue to use these successfully, but modern tools can greatly improve programmer productivity.  Some such tools are especially recommended for the Windows operating system, since it does not support command-line usage as cleanly as Linux or Mac OS.

## Compilers

Compilers are sophisticated software packages.  They must perform a complex analysis of the code, translate it to machine language, and invoke a linker to create an executable.  This "big picture" view and direct machine language output is what enables compiled programs to run generally with much higher performance than interpreted scripts.  Compilers also offer a large number of compile-time options that can significantly impact the performance and sometimes the results of the executable.  

Many compilers are available, but we will focus on those from three vendors.

_Gnu Compiler Collection_
The [Gnu Compiler Collection](https://gcc.gnu.org/) is a well-established, free and open-source bundle. The base compiler is `gcc` for C.  Several add-on languages are supported, the most widely used of which are `g++` (C++) and `gfortran` (Fortran).  

_NVIDIA HPC SDK_
The [NVIDIA HPC SDK](https://developer.nvidia.com/hpc-sdk) is another free (though not open-source) compiler suite for C/C++/Fortran.  Formerly the Portland Group compilers, it is a general-purpose package but is oriented toward extensions for programming NVIDIA GPUs.  For example, it provides Fortran bindings to CUDA.  These compilers are `nvcc`, `nvc++`, and `nvfortran`. 

_Intel Compilers_
The Intel compilers have a reputation for producing the fastest executables on Intel architectures.  Most high-performance computing sites provide the commercial Intel suite `icc`, `icpc`, and `ifort`.  Intel's Parallel Studio package also ships with high-performance Math Kernel Libraries (MKL), MPI (IntelMPI), and threading (tbb).  The Parallel Studio package is available on the UVA HPC system.

Intel has recently released the [oneAPI Toolkits](https://software.intel.com/content/www/us/en/develop/tools/oneapi/all-toolkits.html). They are free but not open source, and are supported only throught a community forum.  

## Integrated Development Environments

An Integrated Development Environment (IDE) combines an editor and a way to compile and run programs in the environment.
A well-known IDE for Microsoft Windows is Visual Studio. This is available through the Microsoft Store; it is not free for individuals.
Mac OS uses Xcode as its native IDE. Xcode includes some compilers, particularly for Swift, but it can manage several other languages.  Available at the App Store and free.

A full-featured cross-platform IDE is [Eclipse] (http://www.eclipse.org/).  Free.

A lighter-weight IDE for Windows and Linux is [Code::Blocks] (http://www.codeblocks.org/).  Free.

An increasingly popular IDE is Visual Studio Code ([VSCode](https://code.visualstudio.com/)) from Microsoft. It is also cross-platform, with versions available for Windows, Mac OS, and Linux.  It does not support C, C++, or Fortran by default; extensions must be installed to provide syntax highlighting and debugging for those languages.  C and C++ are installed with one extension that can be found at the top of the list.  To install a Fortran extension, open the extension panel if it is hidden, and type `fortran` in the search bar.  There are several options; the one simply called "fortran" is popular.  Also recommended are the breakpoint extension and fprettify.

In our examples, we will use a very lightweight IDE called [Geany](https://www.geany.org/) since it is free, easy to install and use, and  works on all three platforms.  It is more of a programmer's editor than a full-featured IDE, but it does include some build tools.

## Building an Executable

Creating an executable is generally a multi-step process.  Of course, the first step is the preparation of a _source file_.  Fortran has two conventions; the older is _fixed format_ and conventionally those source files should end with `.f`.  The newer convention, which we will use for all our examples here, is _free format_ and most compilers expect those files to end in `.f90`, even if the actual standard supported is 2003 or later.

From each source file, the compiler first produces an _object file_.  In Unix these end in `.o`, or `.obj` on Windows.
This is the _compilation_ step.
Object files are binary (machine language) but cannot be executed.  They must be _linked_ into an executable by a program called a _linker_ (also called a _loader_).  The linker is normally invoked through the compiler.  The entire process of compiling and linking is called _building_ the executable.

If not told otherwise a compiler will attempt to compile and link the source file(s) it is instructed to compile.  If more than one file is needed to create the executable, linking will not work until all object files are available, so the compiler must be told to skip that step.
For Unix compilers the `-c` option suppresses linking.  The compiler must then be run again to build the executable from the object files.
The linker option `-o` is used to name the binary something other than `a.out`.

Example:
```
gfortran -c mymain.f90
gfortran -c mysub.f90
```
IDEs generally manage basic compiler options and usually name the executable based on the project name.  Our examples of command line usage will all assume a Unix operating system; there are some differences between Linux and MacOS, with larger differences for Windows.  On MacOS and especially Windows, using an IDE makes code management simpler.  Using Geany as our example, clicking the icon showing a pyramid pointing to a circle will compile the current file without attempting to invoke the linker.  The brick icon builds the current file, so it must be possible to create a standalone executable from a single file.

## Linkers and Libraries

When the executable is created any external libraries must also be linked.
The compiler will search a standard path for libraries.  On Unix this is typically `/usr/lib`, `/usr/lib64`, `/usr/local/lib`, `/lib`.
If you need libraries in other locations, you must give the compiler the path. `-L` followed by a path works, then each library must be named with the pattern `libfoo.a` or `libfoo.so` and be referenced `-lfoo`.  

Example:
```
gfortran –o mycode –L/usr/lib64/foo/lib mymain.o mysub.o -lfoo
```

A library ending in `.a` is _static_.  Its machine-language code will be physically incorporated into the executable.  If the library ends in `.so` it is _dynamic_.  It will be invoked by the executable at runtime.

Many libraries require _include files_, also called _header_ files.  These must be incorporated at compile time.  As for libraries, there is a standard system search path and if the headers are not located in one of those directories, the user must provide the path to the compiler with the `-I` flag.

Example:
```
gfortran –c –I/usr/lib64/foo/include mymain.f90 
```
If the library, or your code, uses modules in addition to or in place of headers, the `I` flag is also used to specify their location.  We will learn about modules and how they interact with your build system later.

The current working directory is included in the library and header paths, but not its subdirectories.

### Compiling and Linking Multiple Files with an IDE

Our discussion of building your code has assumed the use of a command line on Unix.  An IDE can simplify the process even on that platform.
We will use Geany for our example; more sophisticated IDEs have more capabilities, but Geany illustrates the basic functions.

We have two files in our project, [example.f90](/courses/fortran_introduction/compiler_example/example.f90) and [adder.f90](/courses/fortran_introduction/compiler_example/adder.f90).  The main program is `example.f90`.  It needs `adder.f90` to create the executable.  We must open the two files in Geany.  Then we must compile (not build) each one separately.  Once we have successfully compiled both files, we open a terminal window (cmd.exe on Windows).  We navigate to the folder where the files are located and type 
```
gfortran -o example example.o adder.o
```
Notice that we name the executable the same as the main program, minus the file extension.  This follows the Geany convention for the executable.  It is not a requiment but if Geany is to execute it, that is the name for which it will look.

You can run the executable either from the command line (`./example` may be required for Linux) or through the Geany execute menu or gears icon.  
If Geany is to run a multi-file executable then the main program file must be selected as the current file as well as match the name of the executable.

{{< figure src="/courses/fortran_introduction/img/GeanyFiles.png" width=500px caption="Executing the example program" >}}

This process becomes increasingly cumbersome as projects grow in number and complexity of files.  The most common way to manage projects is through the [make](courses/fortran_introduction/make.md) utility, which we will examine later.
