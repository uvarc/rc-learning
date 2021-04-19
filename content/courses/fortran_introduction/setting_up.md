---
title: "Setting Up Your Environment"
toc: true
type: docs
weight: 10

menu:
    fortran_introduction:
        name: Setting Up 
        weight: 10

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

In our examples, we will use a very lightweight IDE called Geany since it is free, easy to install and use, and  works on all three platforms.  It is more of a programmer's editor than an IDE, but it does include some build tools.

## Installing Compilers and an IDE

### Linux

For users of the University of Virginia's cluster, first load a compiler module.
```
module load gcc
```
This command brings a newer gcc, g++, and gfortran into the current environment. Now load Geany.
```
module load geany
geany&
```

For personal use, compilers, Geany, and the other cross-platform IDEs are available for all popular Linux distributions and can be installed through the distribution's package manager or by downloading from the vendor (e.g. the NVIDIA HPC SDK).  Most workstation users do not install modules, so the module command would not be required. 

On Mac and Windows, an IDE can be installed in the usual way for those platforms, either through the application "store" or starting at the package's home page.

### Mac OS

_GCC_
Install Xcode from the App Store.  This will install gcc and g++.
To use Fortran, download a binary for your version of OS from
the [binary site](https://gcc.gnu.org/wiki/GFortranBinaries).  These compilers can be used with any IDE available for Macs.  Xcode does not support Fortran directly so another option is recommended, but it can be used for C and C++.

_Intel oneAPI_
Download the Mac version from Intel.

The NVIDIA HPC SDK is not available for Macs.

Geany can be installed from its [homepage](www.geany.org).  Other options, such as VSCode, can be installed similarly.

### Windows

There are several compiler options for Windows.  Visual Studio supports C and C++ but not Fortran, and no Fortran is currently available for it.  However, there are several free options.

_GCC_

A popular vehicle for using the Gnu compilers on Windows is [Cgwin](https://www.cygwin.com/).  Cygwin also provides a large number of other Unix tools.  The Gnu compiler suite is installed through the Cygwin installer.

Recently, Microsoft has released the Windows Subsystem for Linux ([WSL](https://docs.microsoft.com/en-us/windows/wsl/)).  This is not a virtual machine but is a type of Linux emulator.  It is a full-featured command-line-only Linux environment for Windows, but the X11 graphical user interface is not supported.

A drawback to both Cygwin and the WSL is portability of executables.  Cygwin executables must be able to find the Cygwin DLL and are therefore not standalone.
WSL executables only run on the WSL.  For standalone, native binaries a good choice is _MingGW_.  MinGW is derived from Cygwin.

MinGW provides a free distribution of gcc/g++/gfortran.  The standard MinGW distribution is updated fairly rarely and generates only 32-bit executables.  We will describe [MinGW-w64](http://mingw-w64.org/doku.php), a fork of the original project.
{{< figure src="/courses/fortran_introduction/img/MinGW1.png" width=500px >}}

MinGW-w64 can be installed beginning from the [MSYS2](https://www.msys2.org/) project.  MSYS2 provides a significant subset of the Cygwin tools.
Download and install it.
{{< figure src="/courses/fortran_introduction/img/MSYS2.png" width=500px >}}
Once it has been installed, follow the [instructions](https://www.msys2.org/) to open a command-line tool, update the distribution, then install the compilers and tools.

_Intel oneAPI_
Download and install the basic toolkit and, for Fortran, the HPC toolkit.

_NVIDIA HPC SDK_
Download and install the package.

#### Environment Variables in Windows
To use any of these compilers through an IDE, they must be added to the Path environment variable.  You must use the path you chose for the installation.  The default is C:\msys64\mingw64\bin for the compilers.

Control Panel->System and Security->Advanced system settings->Environment Variables
{{< figure src="/courses/fortran_introduction/img/WindowsEV.png" width=412px >}}
Once you open Path, click New to add to the Path
{{< figure src="/courses/fortran_introduction/img/WindowsPath.png" width=500px >}}

To test that you have successfully updated your path, open a `cmd` window and type
```
gfortran
```
You should see an error
```
gfortran: fatal error: no input files
```

## Compiling Your First Program

We will show Geany and VSCode on Windows.  Both look similar on the other platforms.  

Open Geany (or VSCode).  Type in the following
{{< code file="courses/fortran_introduction/hello.f90" lang=no-hightlight >}}

{{< figure src="/courses/fortran_introduction/img/Geany1.png"  >}}
Syntax coloring will not be enabled until the file is saved with a file extension that corresponds to the language.  Save this file as `hello.f90`.  The coloring will appear.
{{< figure src="/courses/fortran_introduction/img/Geany2.png" width=500px >}}

The appearance is similar in VSCode.
{{< figure src="/courses/fortran_introduction/img/VSCode.png" width=500px >}}

In Geany, click the `Build` icon (a brick wall).  A message confirming a successful compilation should be printed.

{{< figure src="/courses/fortran_introduction/img/Geany3.png" width=500px >}}

Now click the `Execute` button.  A new window will open and the message will be printed.

{{< figure src="/courses/fortran_introduction/img/Geany4.png" width=500px caption="Executing the Hello World program" >}}
