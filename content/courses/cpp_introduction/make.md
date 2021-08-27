---
title: "Managing Projects with Make"
toc: true
type: book
weight: 14

---

`make` is a tool to manage builds, especially with multiple files.
It has a rigid and peculiar syntax.
It will look for a `makefile` first, followed by `Makefile` (on case-sensitive systems).
The `makefile` defines one or more _targets_ .  The target is the product of one or more _rules_ .
The target is defined with a colon following its name.  If there are _dependencies_ those follow the colon.
Dependencies are other files that are required to create the current target.

### Targets and Rules

Example:
```
myexec:main.o module.o
<tab>g++ -o myexecmain.o module.o
```
The tab is _required_ in the rule.  Don’t ask why.

Macros (automatic targets) for rules:
```
$@ the file name of the current target
$< the name of the first prerequisite
```

### Variables and Comments

We can define variables in makefiles:
```
CC =gcc
CXX=g++
```
We then refer to them as `$(CC)`,`$(CXX)`, etc.
Common variables: F90, CC, CXX, FFLAGS, F90FLAGS, CFLAGS, CXXFLAGS, CPPFLAGS (for the preprocessor), LDFLAGS.

The continuation marker `\` (backslash) can be used across multiple lines. It _must_ be the last character on the line; do not add spaces after it.

Comments are indicated by the hash mark `#`.  Anything beyond it will be ignored except for a continuation marker, which will extend the comment.

### Suffix Rules

If all .cxx (or .cc or whatever) files are to be compiled the same way, we can write a _suffix rule_ to handle them.
It uses a _phony target_ called `.SUFFIXES`.
```
.SUFFIXES: .cxx .o
	$(CXX) -c $(CXXFLAGS) –c $<
```

### Pattern Rules

This is an extension by Gnu make (gmake), but nearly every `make` is `gmake` now.
It is similar to suffix rules.  
The pattern for creating the .o:
```
%.o: %.cxx
	$(CXX) $(CXXFLAGS) -c $<
```

Example:
{{< code-download file="/courses/cpp_introduction/codes/Makefile" lang="make" >}}

In this example, notice that the suffix rule applies the global compiler flags and explicitly includes the `-c` option.  If a particular file does not fit this pattern, a rule can be written for it and it will override the suffix rule.  The link rule includes the loader flags and the `-o` flag.  The compilation suffix rule uses the special symbol for the prequisite; the link rule applies to the current target.

The example also demonstrates switching back and forth between "debug" and "optimized" versions.  The "debug" version would be created this time.  The `-g` flag is required for debugging.  The `-C` flag is a very useful flag specific to Fortran that enables bounds checking for arrays.  Both these flags, but especially `-C`, will create a slower, sometimes much slower, executable, so when debugging is completed, always recompile with the optimization flag or flags enabled, of which `-O` is the minimum and will activate the compiler's default level.  You must _always_ `make clean` anytime you change compiler options.

For further reading about `make`, see the [gmake documentation](https://www.gnu.org/software/make/manual/).

#### Makemake

Makemake is a Perl script first developed by Michael Wester soon after the introduction of Fortran 90, in order to construct correct makefiles for modern Fortran code.  The version supplied here has been extended to work for C and C++ codes as well.  It is freely licensed but if you use it, please do not remove the credits at the top.

[makemake](/courses/cpp_introduction/codes/makemake)

This version works reasonably well for Fortran, C, and C++.  It will generate stubs for all languages. You may remove any you are not using.  Also note that the output is a skeleton `Makefile`.  You must at minimum name your executable, and you must fill in any other options and flags you wish to use.  The `makemake` script blindly adds any files ending in the specified suffixes it finds in the current working directory whether they are independently compilable or not, so keep your files organized, and be sure to edit your Makefile if you have files you need but cannot be compiled individually.

Several other build tools, some called `makemake`, are available and may be newer and better supported.  See [here](https://github.com/OutsourcedGuru/makemake) for example.  It also produces files for [CMake](https://cmake.org), a popular build system, especially for Windows.

### Building with an IDE and a Makefile

Several IDEs will manage multiple files as a "project" and will generate a Makefile automatically.  Unfortunately, that Makefile is frequently incorrect for Fortran codes that use modules, so you may have to write your own Makefiles.  The `makemake` script or one of the newer build tools described above can help.

We will use the NetCDF library as an example.  Environmental sciences still use Fortran a great deal and this is a popular library for data files. The example code is taken from their standard examples.  The file are [simple_xy_wr.cpp](/courses/cpp_introduction/netcdf_example/simple_xy_wr.cpp).

On our test system, the library is installed in a standard location, but the netcdf module is not, so we need to use the `-I` flag but not the `-L` flag.
First we run makemake to obtain a skeleton Makefile.

{{< code file="/courses/cpp_introduction/netcdf_example/Makefile.sample" lang='make' >}}

We edit it to add the addition information required and to remove unneeded lines.

{{< code-download file="/courses/cpp_introduction/netcdf_example/Makefile" lang="make" >}}

#### Make with MinGW/MSYS2 on Windows

The MinGW64/MSYS2 system provides two versions of `make`.  In newer releases, on newer Windows, either should work.  If you do not wish to add an additional path to your PATH [environment variable](/courses/cpp_introduction/setting-up) use `mingw32-make`.  You can change the Geany build commands through its [build tools]() menu.  The mingw32-make tool may not support as many features as the full-fledged Gnu `make` provided by MSYS2. You can use Gnu make by adding the folder `C:\msys64\usr\bin` to your PATH variable.  This would not require changing the build tool on Geany.

Exercise 1:
If you have not already done so, download or copy the [example.cxx](/courses/cpp_introduction/compiler_example/example.cxx) and its required [adder.cxx](/courses/cpp_introduction/compiler_example/adder.cxx).  Place them into a separate folder.  Run `makemake`.  Edit the Makefile appropriately.  Build the project using Geany or your choice of IDE.

Exercise 2:
If you are working on a system with NetCDF available, download the two files and the completed Makefile into their own folder.  Open Geany and browse to the location of the files.  Open the two source files.  Either select `Make` from the `Build` menu, or from the dropdown arrow next to the brick icon choose `Make All`.
Build the code using the Makefile.  Execute the result.

