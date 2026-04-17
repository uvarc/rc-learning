---
title: Building an Executable
date: 2026-04-14T16:42:59Z
type: docs 
weight: 100
menu: 
    building-running-c-cpp-fortran:
---

Users who write their own programs in C, C++, or Fortran, or who use community codes written in these languages, must always convert their source code into a runnable executable before they can use the software.  The process of creating an executable is called _building_, and consists of _compiling_, or converting source into machine-language files, and _linking_, which links these files along with any external libraries into an executable.

## Compilers

A _compiler_ is a program that converts human-written _source code_ directly into a standalone program called an _executable_ (or binary).  This is in contrast to an _interpreter_, which is a program that executes source code, often called a _script_ in this case, line by line.  

Compilers go through a multi-stage process to convert source code to an executable. 

When running an interpreter, the executable is the interpreter itself. Your script cannot be run directly.  Some scripts can invoke their own interpreters and run standalone, but they are not themselves binaries.

## Linkers

The compiler first produces an *object file* for each *source file*. In Unix these end in .o 

Object files are binary (machine language) but cannot be executed. They must be *linked* into an executable.

Libraries are special archives of compiled code that can be invoked through their application programming interface, or _API_.  Like the object files, they must be linked into an executable in order to be utilized.  

The program that generates the executable from object files and any external libraries is the _linker_ (also called a loader). Linkders are nearly always invoked through the compiler, not separately. The linker joins all object files as specified, and if run through the appropriate compiler it also links the compiler's *runtime libraries* for the source language. These are libraries used to carry out procedures intrinsic to the language. This happens automatically if the compiler matches the language of the main program; it is not necessary to add the runtime libraries explicitly.  However, in mixed-language programming it may be necessary to add them to the linking instructions.

## Building

The process of compiling and linking is usually called _buildint_ the executable.  The result is a file in machine language which cannot be read by (most) humans.  Binaries/executables are specific to a platform, a combination of machine architecture and operating system. You cannot run a Windows binary on a Linux system, and vice versa.
