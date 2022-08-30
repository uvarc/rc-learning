---
title: "Setting Up Your Environment"
toc: true
type: book
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

For personal use, compilers, Geany, and the other cross-platform IDEs are available for all popular Linux distributions and can be installed through the distribution's package manager or by downloading from the vendor (e.g. the NVIDIA HPC SDK).  Most workstation users do not install environment modules, so the module command would not be required.  However, it may be convenient if you wish to instal multiple compilers.  At least one implementation of software modules is usually available for a given distribution.

_GCC_
The gcc compiler is a standard part of all Linux distributions.  However, it is usually necessary to add g++ and gfortran separately.

_Intel OneAPI_
Download the Linux version from [Intel](https://www.intel.com/content/www/us/en/developer/tools/oneapi/toolkits.html).  Installing the HPC Toolkit as well as the required Basic Toolkit is recommended.

_NVIDIA HPC SDK_
Download and install the [package](https://developer.nvidia.com/nvidia-hpc-sdk-downloads).  If your distribution is not supported with a package, you may have t
o download the tarball and set paths appropriately.

To use a different compiler with Geany, from the `Build` menu choose `Set Build Commands`. To use a different compiler with Geany, from the `Build` menu choose `Set Build Commands`.  See the chapter on [building](/courses/cpp_introduction/building) your codes for compiler names and some options.

### IDEs for Mac and Windows

On Mac and Windows, an IDE can be installed in the usual way for those platforms, either through the application "store" or starting at the package's home page.

### Mac OS

_GCC_
Install Xcode from the App Store.  This will install gcc and g++.  You may also wish to install the gcc package from [homebrew](https://brew.sh) since it will install the Xcode command-line tools for you.

_Intel oneAPI_
Download the Mac version from [Intel](https://www.intel.com/content/www/us/en/developer/tools/oneapi/toolkits.html).  It may be useful to install the HPC Toolkit as well as the required Basic Toolkit. 

The NVIDIA HPC SDK is not available for Macs.

Geany can be installed from its [homepage](www.geany.org).  Other options, such as VSCode, can be installed similarly.

### Windows

There are several compiler options for Windows.  Visual Studio supports C and C++.  There are also several free options.

_GCC_

A popular vehicle for using the Gnu compilers on Windows is [Cgwin](https://www.cygwin.com/).  Cygwin also provides a large number of other Unix tools.  The Gnu compiler suite is installed through the Cygwin installer.

Recently, Microsoft has released the Windows Subsystem for Linux ([WSL](https://docs.microsoft.com/en-us/windows/wsl/)).  This is not a virtual machine but is a type of Linux emulator.  It is a full-featured command-line-only Linux environment for Windows, but the X11 graphical user interface is not supported.

A drawback to both Cygwin and the WSL is portability of executables.  Cygwin executables must be able to find the Cygwin DLL and are therefore not standalone.
WSL executables only run on the WSL.  For standalone, native binaries a good choice is _MingGW_.  MinGW is derived from Cygwin.

MinGW provides a free distribution of gcc/g++/gfortran.  The standard MinGW distribution is updated fairly rarely and generates only 32-bit executables.  We will describe [MinGW-w64](http://mingw-w64.org/doku.php), a fork of the original project.
{{< figure src="/courses/cpp_introduction/img/MinGW1.png" width=500px >}}

MinGW-w64 can be installed beginning from the [MSYS2](https://www.msys2.org/) project.  MSYS2 provides a significant subset of the Cygwin tools.
Download and install it.
{{< figure src="/courses/cpp_introduction/img/MSYS2.png" width=500px >}}
Once it has been installed, follow the [instructions](https://www.msys2.org/) to open a command-line tool, update the distribution, then install the compilers and tools.

_Intel oneAPI_
First install [Visual Studio](https://visualstudio.microsoft.com/vs/community/).
  Individual developers and most academic users are eligible for the free commun
ity edition.  After installing VS, download and install the Basic Toolkit.  The HPC Toolkit is also recommended; it is installed after Basic.

_NVIDIA HPC SDK_
Download and install the package when it is available.

#### Environment Variables in Windows
To use any of these compilers through an IDE, they must be added to the Path environment variable.  You must use the path you chose for the installation.  The default is C:\msys64\mingw64\bin for the compilers.

Control Panel->System and Security->Advanced system settings->Environment Variables
{{< figure src="/courses/cpp_introduction/img/WindowsEV.png" width=412px >}}
Once you open Path, click New to add to the Path
{{< figure src="/courses/cpp_introduction/img/WindowsPath.png" width=500px >}}

To test that you have successfully updated your path, open a `cmd` window and type
```
g++
```
You should see an error
```
g++: fatal error: no input files
```

## Compiling Your First Program

We will show Geany and VSCode on Windows.  Both look similar on the other platforms.  

Open Geany (or VSCode).  Type in the following
{{< code file="courses/cpp_introduction/codes/hello.cxx" lang=no-hightlight >}}

{{< figure src="/courses/cpp_introduction/img/Geany1.png" width=500px  >}}
Syntax coloring will not be enabled until the file is saved with a file extension that corresponds to the language.  Save this file as `hello.cxx`.  The coloring will appear.
{{< figure src="/courses/cpp_introduction/img/Geany2.png" width=500px >}}

The appearance is similar in VSCode.
{{< figure src="/courses/cpp_introduction/img/VSCode.png" width=500px >}}

In Geany, click the `Build` icon (a brick wall).  A message confirming a successful compilation should be printed.

{{< figure src="/courses/cpp_introduction/img/Geany3.png" width=500px >}}

Now click the `Execute` button.  A new window will open and the message will be printed.

{{< figure src="/courses/cpp_introduction/img/Geany4.png" width=500px caption="Executing the Hello World program" >}}

### Build Commands in Geany

If you wish to use a different compiler suite from the default GCC with Geany, or if you need to change the compiling or linking commands for any other reason, you can modify the build commands through the Build->Set Build Commands dialogue.
{{< figure src="/courses/cpp_introduction/img/GeanyBuildTools.png" width=500px caption="Edit the build commands Geany will use" >}}
