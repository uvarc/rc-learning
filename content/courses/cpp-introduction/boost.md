---
date : "2021-06-23T00:00:00"
title: "The Boost Template Library"
toc: true
type: book
weight: 55

---

One of the most popular add-on libraries for C++, especially numerical or scientific programming, is [Boost](https://www.boost.org/).  Boost is not included with any compilers, but is generally easy to obtain.  For Linux systems generally it is available through the package manager.  

### Installing Boost

Boost uses its own build system on Unix and Mac OS.  On Windows there is an experimental CMake system but generally the "bjam" builder is still used.
In all cases, installing Boost requires some familiarity with using a command line.

#### Unix and Mac OS

On a Linux system, the simplest way to install Boost is to utilize the package manager of the distribution.  For example on Ubunutu the command is
```no-highlight
sudo apt install libboost-all-dev
```
or on Fedora (or Centos 8 and beyond)
```no-highlight
sudo dnf install boost
sudo dnf install boost-devel
```
If you do not have superuser (sudo) permission or you wish to install Boost somewhere other than the main system libraries, follow the general [instructions](
https://www.boost.org/doc/libs/1_76_0/more/getting_started/unix-variants.html).
The default prefix for installation from `b2 install` is `/usr/local`.  This is true for both Linux and Mac OS.  If you wish to install to `/usr/local`, which is normally in the system search paths, you will need to run the installation command with sudo
```no-highlight
cd path/to/boost/source
./bootstrap.sh
sudo ./b2 install
```
If you do not have sudo privileges or you wish to install it someplace else, keep in mind that this will affect how you reference the headers and libraries with `-I` and `-L`.  You must provide the prefix to the installation directory to the `bootstrap.sh` script to install someplace other than the default location.
```no-highlight
./bootstrap.sh --prefix=/home/mst3k/boost
./b2 install
```

#### Windows

Installation on Windows is somewhat more complicated than on Unix variants.  First download the zipped source file.
1. Start a command window. If you wish to install Boost in a standard search path, you will need to run it as administrator.
2. Unzip the .zip file into a folder of your choice `C:\yourpath`.  It will unpack to a folder with a version number, which may vary from our example `C:\yourpath\boost_1_76_0`
3. Create some folders, e.g.
      mkdir C:\boost-build
      mkdir C:\yourpath\boost_1_76_0\boost-build
      mkdir C:\boost
4. From the command prompt, `cd C:\yourpath\boost_1_76_0\tools\build
5. Run `boostrap.bat gcc`
6. Install the build system with `b2 --prefix="C:\boost-build" install`
7. Modify your session PATH with `set PATH=%PATH%;C:\boost-build\bin`
8. Return to your source directory `cd C:\yourpath\boost_1_76_0`
9. Build with `b2 --build-dir="C:\install\boost --prefix="C:\boost" --build-type=complete toolset=gcc install`
10. This will install to "C:\boost\include\boost-1_76_0" and "C:\boost\lib"
      You may remove temporary unpacking and build directories if you wish.  You may also move the header files up to C:\boost\include if you prefer.  Remember that `<boost/boostheader.hpp>` will use `-I` to start looking for that subdirectory.

Using Boost is probably simplest with a Makefile.  The [example](/courses/cpp-introduction/codes/makefile.windows_boost) is for Windows with a particular choice of location for the boost header files and libraries.  Change the locations for `-I` and `-L` as appropriate if they are located elsewhere on your system.  Please refer to the earlier [chapter](/courses/cpp-introduction/make) for a review of setting up Makefiles.  This example goes with a standard Boost [example](/courses/cpp-introduction/codes/boost_example.cxx).

If you have installed Boost onto a Linux or Mac OS system to a system default search location such as `/usr` or `/usr/local` you will not need to specify the `-I` or `-L` paths at all.  The example [makefile](/courses/cpp-introduction/codes/makefile.linux_mac_boost) assumes installation in system paths in the compiler's default search paths.

## Boost MultiArrays

C-style arrays are widely used but lack some important features, such as the ability to check that index references do not occur outside of the arrays bounds.
The C++ STL array type has more features, but is limited to one dimension.  A workaround is to "stack" vectors, since a vector of a vector amounts to a 2-dimensional structure.
```c++
#include <vector>
using namespace std;

   vector<vector<double>> A;
```

The Boost library provides a popular alternative, the MultiArray. This structure can be N-dimensional, its bounds (extents) can be checked, and it can be reshaped and resized.  The price is that it can be slow.

{{< code-download file="/courses/cpp-introduction/codes/boost_array.cxx" lang="c++" >}}

**Exercise**

If you have succeeded in installing Boost, or you have access to a system where it has been installed, download and run the above boost_array.cxx program.
