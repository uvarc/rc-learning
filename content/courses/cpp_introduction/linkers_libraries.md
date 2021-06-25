---
title: "Linkers and Libraries"
toc: true
type: book
weight: 13

menu:
    cpp_introduction:
        parent: Building and Running Your Programs
        weight: 13

---

## Linkers and Libraries

When the executable is created any external libraries must also be linked.
The compiler will search a standard path for libraries.  On Unix this is typically `/usr/lib`, `/usr/lib64`, `/usr/local/lib`, `/lib`.
If you need libraries in other locations, you must give the compiler the path. `-L` followed by a path works, then each library must be named with the pattern `libfoo.a` or `libfoo.so` and be referenced `-lfoo`.  

Example:
```
g++ –o mycode –L/usr/lib64/foo/lib mymain.o mysub.o -lfoo
```

A library ending in `.a` is _static_.  Its machine-language code will be physically incorporated into the executable.  If the library ends in `.so` it is _dynamic_.  It will be invoked by the executable at runtime.

Many libraries require _include files_, also called _header_ files.  These must be incorporated at compile time.  As for libraries, there is a standard system search path and if the headers are not located in one of those directories, the user must provide the path to the compiler with the `-I` flag.

Example:
```
g++ –c –I/usr/lib64/foo/include mymain.f90 
```
If the library, or your code, uses modules in addition to or in place of headers, the `I` flag is also used to specify their location.  We will learn about modules and how they interact with your build system later.

The current working directory is included in the library and header paths, but not its subdirectories.

MacOS is similar to Unix but generally uses different paths for the system libraries.

Working with libraries on Windows is somewhat more complex.  Dynamic libraries are called DLLs (dynamically linked libraries) and end in `.dll`, but generally code links an "import library" that ends in `.lib`.  Although the underlying DLL is supposed to be universal, these import library files are compiler dependent.  Microsoft Visual Studio will automatically link to the .lib but MingGW uses a more Unix-like format by default and expects a `.a` suffix.  Libraries provided by Windows will be in MSVC (Microsoft Visual C++) format so translation may be required.  MinGW-64 provides the `gendef` tool that can create a definition file from a DLL, which can then be used to produce a MinGW import library.  See the MinGW-64 [documentation](https://sourceforge.net/p/mingw-w64/wiki2/gendef/) for specifics.  The MinGW-64 [FAQ](https://sourceforge.net/p/mingw-w64/wiki2/FAQ/) is also helpful.

### Compiling and Linking Multiple Files with an IDE

Our discussion of building your code has assumed the use of a command line on Unix.  An IDE can simplify the process even on that platform.
We will use Geany for our example; more sophisticated IDEs have more capabilities, but Geany illustrates the basic functions.
Using Microsoft tools such as Visual Studio or VSCode on Windows may be desirable if you will need to link to Microsoft or other vendor-provided libraries.

We have three files in our project, [example.cxx](/courses/cpp_introduction/compiler_example/example.cxx), [adder.cxx](/courses/cpp_introduction/compiler_example/adder.cxx) and [adder.h](/courses/cpp_introduction/compiler_example/adder.h).  The main program is `example.cxx`.  It needs `adder.cxx` to create the executable.  We must open the two files in Geany.  Then we must compile (not build) each one separately.  Once we have successfully compiled both files, we open a terminal window (cmd.exe on Windows).  We navigate to the folder where the files are located and type 
```
g++ -o example example.o adder.o
```
Notice that we name the executable the same as the main program, minus the file extension.  This follows the Geany convention for the executable.  It is not a requiment but if Geany is to execute it, that is the name for which it will look.

You can run the executable either from the command line (`./example` may be required for Linux) or through the Geany execute menu or gears icon.  
If Geany is to run a multi-file executable then the main program file must be selected as the current file as well as match the name of the executable.

{{< figure src="/courses/cpp_introduction/img/GeanyFiles.png" width=500px caption="Executing the example program" >}}

The file ending in `.h` is called a _header file_. The best practice in C++ is to separate the _interface_ into a header file and the _implementation_ into the source file. We will discuss this, as well as the preprocessor commands, when we cover functions.

This process becomes increasingly cumbersome as projects grow in number and complexity of files.  The most common way to manage projects is through the [make](courses/cpp_introduction/make.md) utility, which we will examine next.
