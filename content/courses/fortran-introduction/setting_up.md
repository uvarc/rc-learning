---
title: "Setting Up Your Environment"
date : "2021-04-5T00:00:00-05:00"
toc: true
type: book
weight: 12

menu:
    fortran-introduction:
        parent: Building and Running Your Programs
        weight: 12

---

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

For personal use, compilers, Geany, and the other cross-platform IDEs are available for all popular Linux distributions and can be installed through the distribution's package manager or by downloading from the vendor (e.g. the NVIDIA HPC SDK).  Most workstation users do not install environment modules, so the module command would not be required. 

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

MinGW provides a free distribution of gcc/g++/gfortran.  The standard MinGW distribution is updated fairly rarely and generates only 32-bit executables.  We will describe [MinGW-w64](https://www.mingw-w64.org/), a fork of the original project.
{{< figure src="/courses/fortran-introduction/img/MinGW1.png" width=500px >}}

MinGW-w64 can be installed beginning from the [MSYS2](https://www.msys2.org/) project.  MSYS2 provides a significant subset of the Cygwin tools.  Download and install it.
{{< figure src="/courses/fortran-introduction/img/MSYS2.png" width=500px >}}
Once it has been installed, follow the [instructions](https://www.msys2.org/) to open a command-line tool, update the distribution, then install the compilers and tools. For Fortran users, the `mingw64` repository may be preferable to the `ucrt64` repo. To find packages, visit their [repository](https://packages.msys2.org/package/). 

A discussion of installing MinGW-64 compilers for use with VSCode has been posted by Microsoft [here](https://code.visualstudio.com/docs/cpp/config-mingw). To use mingw64 rather than ucrt64, simply substitute the text string. Fortran users should install both the C/C++ and Fortran extensions for VSCode.

_Intel oneAPI_
Download and install the basic toolkit and, for Fortran, the HPC toolkit.

_NVIDIA HPC SDK_
Download and install the package when it is available.

#### Environment Variables in Windows
To use any of these compilers through an IDE, they must be added to the Path environment variable.  You must use the path you chose for the installation.  The default is C:\msys64\mingw64\bin for the compilers.

Control Panel->System and Security->Advanced system settings->Environment Variables
{{< figure src="/courses/fortran-introduction/img/WindowsEV.png" width=412px >}}
Once you open Path, click New to add to the Path
{{< figure src="/courses/fortran-introduction/img/WindowsPath.png" width=500px >}}

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
{{< code file="courses/fortran-introduction/codes/hello.f90" lang=no-highlight >}}

{{< figure src="/courses/fortran-introduction/img/Geany1.png" width=500px  >}}
Syntax coloring will not be enabled until the file is saved with a file extension that corresponds to the language.  Save this file as `hello.f90`.  The coloring will appear.
{{< figure src="/courses/fortran-introduction/img/Geany2.png" width=500px >}}

The appearance is similar in VSCode.
{{< figure src="/courses/fortran-introduction/img/VSCode.png" width=500px >}}

In Geany, click the `Build` icon (a brick wall).  A message confirming a successful compilation should be printed.

{{< figure src="/courses/fortran-introduction/img/Geany3.png" width=500px >}}

Now click the `Execute` button.  A new window will open and the message will be printed.

{{< figure src="/courses/fortran-introduction/img/Geany4.png" width=500px caption="Executing the Hello World program" >}}
