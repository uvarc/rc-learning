---
title: "Pointers and Memory Management"
toc: true
type: book
weight: 43

menu:
    cpp_introduction:
        parent: Pointers and Memory Management
        weight: 43
---

We have learned that variables are "nicknames" for the contents of a specific location in memory.  In many languages, including C++, we can also define variable that contain the actual address of a memory location.  These variables are called _pointers_ because they "point to" memory directly.

Since an ordinary variable is also associated with memory, we can use the **reference opearator** `&` to obtain the location in memory.  Inversely, given a location, we can use the **dereference operator** `*` to get the value stored there.

{{< code file="/courses/cpp_introduction/codes/ref_deref.cxx" lang="c++" >}}

## Pointer Variables

Pointer variables are declared using the dereference operator.  
```c++
int* p;
```
The type of `p` is _pointer to int_.  

Spaces are ignored by the compiler, so
```c++
int* p;
int *p;
```
and even
```c++
int * p;
```
are equivalent.  However, be aware that
```c++
int* p, i, j;
```
does _not_ declare pointers to int `p`,`i`, and `j` in C++.  Only `p` is a pointer in this declaration.  For this reason it is recommended to keep declarations of pointers separate.

The choice between `int* p` and `int *p` is largely a matter of taste and emphasis.  Some programmers believe that `int* p` is more appropriate for C++ whereas `int *p` is more "C like."  However, there is no rule.  As for curly braces, programmers should be consistent in notation.

Example:
{{< code-download file="/courses/cpp_introduction/codes/pointers.cxx" lang="c++" >}}

## Memory Allocation and C-Style Arrays

Arrays may be sized at runtime.

intN;

N=30;

float* A=new float[N];

The*indicates thatAis a _pointer_ to a block offloatvariables.  We do not have to use it subsequently, and can still refer toAby index, e.g.A[2].

# Multidimensional Arrays with New

* We will only discuss 2d arrays here.
* Two-dimensional arrays are 1-d arrays of pointers to an array.
* intnrows,ncols;
  * //Setnrows,ncolsby some means
* float **A;
* A=new float*[nrows];
* for (inti=0;i<nrows;++i) {
  * A[i]=new float[ncols];
* }

