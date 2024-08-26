---
title: "Managing Projects with Make"
date : "2021-04-5T00:00:00-05:00"
toc: true
type: book
weight: 14

menu:
    fortran-introduction:
        parent: Managing Project with Make
        weight: 14

---

`make` is a tool to manage builds, especially with multiple files.
It has a rigid and peculiar syntax.
It will look for a `makefile` first, followed by `Makefile` (on case-sensitive systems).
The `makefile` defines one or more _targets_ .  The target is the product of one or more _rules_ .
The target is defined with a colon following its name.  If there are _dependencies_ those follow the colon.
Dependencies are other files that are required to create the current target.

### Targets and Rules

**Example**
```
myexec:main.o module.o
<tab>gfortran -o myexecmain.o module.o
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
F90=gfortran

CXX=g++
```
We then refer to them as `$(F90)`,`$(CXX)`, etc.
Common variables: F90, CC, CXX, FFLAGS, F90FLAGS, CFLAGS, CXXFLAGS, CPPFLAGS (for the preprocessor), LDFLAGS.

The continuation marker `\` (backslash) can be used across multiple lines. It _must_ be the last character on the line; do not add spaces after it.

Comments are indicated by the hash mark `#`.  Anything beyond it will be ignored except for a continuation marker, which will extend the comment.

### Suffix Rules

If all .f90 (or .cc or whatever) files are to be compiled the same way, we can write a _suffix rule_ to handle them.
It uses a _phony target_ called `.SUFFIXES`.
```
.SUFFIXES: .f90 .o
	$(F90) -c $(F90FLAGS) –c $<
```

### Pattern Rules

This is an extension by Gnu make (gmake), but nearly every `make` is `gmake` now.
It is similar to suffix rules.  Useful for Fortran 90+:
```
%.mod: %.o
```
Pattern for creating the .o:
```
%.o: %.f90
	$(F90) $(F90FLAGS) -c $<
```

**Example**
{{< code-download file="/courses/fortran-introduction/codes/Makefile" lang="make" >}}

In this example, notice that the suffix rule applies the global compiler flags and explicitly includes the `-c` option.  If a particular file does not fit this pattern, a rule can be written for it and it will override the suffix rule.  The link rule includes the loader flags and the `-o` flag.  The compilation suffix rule uses the special symbol for the prerequisite; the link rule applies to the current target.

The example also demonstrates switching back and forth between "debug" and "optimized" versions.  The "debug" version would be created this time.  The `-g` flag is required for debugging.  The `-C` flag is a very useful flag specific to Fortran that enables bounds checking for arrays.  Both these flags, but especially `-C`, will create a slower, sometimes much slower, executable, so when debugging is completed, always recompile with the optimization flag or flags enabled, of which `-O` is the minimum and will activate the compiler's default level.  You must _always_ `make clean` anytime you change compiler options.

For further reading about `make`, see the [gmake documentation](https://www.gnu.org/software/make/manual/).

#### Makemake

Makemake is a Perl script first developed by Michael Wester soon after the introduction of Fortran 90, in order to construct correct makefiles for modern Fortran code.  The version supplied here has been extended.  It is freely licensed but if you use it, please do not remove the credits at the top.

[makemake](/courses/fortran-introduction/codes/makemake)

This version works reasonably well for Fortran, C, and C++.  It will generate stubs for all languages. You may remove any you are not using.  Also note that the output is a skeleton `Makefile`.  You must at minimum name your executable, and you must fill in any other options and flags you wish to use.  The `makemake` script blindly adds any files ending in the specified suffixes it finds in the current working directory whether they are independently compilable or not, so keep your files organized, and be sure to edit your Makefile if you have files you need but cannot be compiled individually.

Several other build tools called `makemake` are available, and not all handle Fortran.  In addition, more tools have been created since the first `makemake`.  Several options are described at the [Fortran Wiki](http://fortranwiki.org/fortran/show/Build+tools).  The Python `makemake` can search recursively through subfolders, which the original `makemake` cannot.

### Building with an IDE and a Makefile

Several IDEs will manage multiple files as a "project" and will generate a Makefile automatically.  Unfortunately, that Makefile is frequently incorrect for Fortran codes that use modules, so you may have to write your own Makefiles.  The `makemake` script or one of the newer build tools described above can help.

We will use the NetCDF library as an example.  Environmental sciences still use Fortran a great deal and this is a popular library for data files.  The example code is taken from their standard examples, modified to place the subroutine into a separate file.  The file is [simple_xy_wr.f90](/courses/fortran-introduction/netcdf_example/simple_xy_wr.f90).

On our test system, the library is installed in a standard location, but the netcdf module is not, so we need to use the `-I` flag but not the `-L` flag.
First we run makemake to obtain a skeleton Makefile.

{{< code file="/courses/fortran-introduction/netcdf_example/Makefile.sample" lang='make' >}}

We edit it to add the addition information required and to remove unneeded lines.

{{< code-download file="/courses/fortran-introduction/netcdf_example/Makefile" lang="make" >}}

Exercise 1:
If you have not already done so, download or copy the [example.f90](/courses/fortran-introduction/compiler_example/example.f90) and its required [adder.f90](/courses/fortran-introduction/compiler_example/adder.f90).  Place them into a separate folder.  Run `makemake`.  Edit the Makefile appropriately.  Build the project using Geany or your choice of IDE.

Exercise 2:
If you are working on a system with NetCDF available, download the two files and the completed Makefile into their own folder.  Open Geany and browse to the location of the files.  Open the two source files.  Either select `Make` from the `Build` menu, or from the dropdown arrow next to the brick icon choose `Make All`.
Build the code using the Makefile.  Execute the result.
