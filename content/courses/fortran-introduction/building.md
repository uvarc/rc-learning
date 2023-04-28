---
title: "Compilers and IDEs"
toc: true
type: book
weight: 11

menu:
    fortran-introduction:
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

Intel has recently released the [oneAPI Toolkits](https://software.intel.com/content/www/us/en/develop/tools/oneapi/all-toolkits.html). They are free but not open source, and are supported only through a community forum.  In order to obtain the "classic" compilers described above, the HPC Toolkit must be installed.  The newer compilers provided in the Base Toolkit for Intel are `icx`, and `icpx`.  Both the classic and the new Fortran compilers `ifort` and `ifx` are in the HPC Toolkit.

## Integrated Development Environments

An Integrated Development Environment (IDE) combines an editor and a way to compile and run programs in the environment.
A well-known IDE for Microsoft Windows is Visual Studio. This is available through the Microsoft Store; it is not free for individuals.
Mac OS uses Xcode as its native IDE. Xcode includes some compilers, particularly for Swift, but it can manage several other languages.  Available at the App Store and free.

A full-featured cross-platform IDE is [Eclipse] (http://www.eclipse.org/).  Free.

A lighter-weight IDE for Windows and Linux is [Code::Blocks] (http://www.codeblocks.org/).  Free.

Windows programmers using Intel's oneAPI distribution must also install [Visual Studio](https://visualstudio.microsoft.com/).

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
Unix and MacOS do not care about file extensions, but Windows will expect an executable to end in `.exe`.

### Command Line

For full control and access to more compiler options, we can build from the comm
and line.  For Linux and Mac this is a terminal application.  On windows, use a
command prompt for gcc.  The Intel oneAPI distribution ships with an integrated
command prompt in its folder in the applications menu; this command prompt is aw
are of the location of the compiler executables and libraries.

**Example**
```
gfortran -c mycode.f90
gfortran -c mysub.f90
gfortran -o mycode mycode.o mysub.o
```
IDEs generally manage basic compiler options and usually name the executable based on the project name.  Our examples of command line usage will all assume a Unix operating system; there are some differences between Linux and MacOS, with larger differences for Windows.  On MacOS and especially Windows, using an IDE makes code management simpler.  Using Geany as our example, clicking the icon showing a pyramid pointing to a circle will compile the current file without attempting to invoke the linker.  The brick icon builds the current file, so it must be possible to create a standalone executable from a single file for that icon to work.
