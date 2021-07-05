---
title: "Pointers and Memory Management"
toc: true
type: book
weight: 42

menu:
    cpp_introduction:
        parent: Pointers and Memory Management
        weight: 42
---

We have learned that variables are "nicknames" for the contents of a specific location in memory.  In many languages, including C++, we can also define variable that contain the actual address of a memory location.  These variables are called _pointers_ because they "point to" memory directly.

Since an ordinary variable is also associated with memory, we can use the **reference opearator** `&` to obtain the location in memory.  Inversely, given a location, we can use the **dereference operator** `*` to get the value stored there.

{{< codes file="/courses/cpp_introduction/codes/ref_deref.cxx" lang="c++" >}}

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
{{< code-download file="/courses/cpp_introduction/codes/pointer.cxx" lang="c++" >}}

## Memory Allocation and C-Style Arrays

One of the most frequent applications of pointers is to dynamically allocate not single variables, but _blocks_ of memory.  This memory can hold a variety of data structures, but the simplest is a group of variables of the same type.  This is frequently called an _array_.

### Terminology

A _scalar_ is a single item (real/float, integer, character/string, complex, etc.)
An _array_ contains data of the __same type__ with each scalar element addressed by _indexing_ into the array.
An array has one or more _dimensions_ .  The _bounds_ are the lowest and highest indexes.  The _rank_ is the number of dimensions.

A C-style array is nothing more than a block of memory that can be interpreted as an array; it is not a defined data type.  Other options are available in class libraries.

Arrays must be declared by type and either by size or by some indication of the number of dimensions.
```c++
float a[100];
int M[10][10];
```
If a variable is used, it must be a const
```c++
const int N=10;
float z[N];
```
The starting index is always 0, so for a 100-element array the items are numbered 0 to 99.

### Orientation

An advantage of arrays is that the elements are _adjacent_ in memory and are arranged linearly no matter how many dimensions you declare.  If you declare a 3x2 array the order in memory is
```no-highlight
(1,1), (1,2), (2,1), (2,2), (3,1), (3,2)
```
“Orientation” refers to how the array is stored _in_  _memory_ , not to any mathematical properties.
C++ and most other languages are _row-major_ oriented.  Some (Fortran,Matlab, R) are _column-major_ oriented.
Loop indices should reflect this whenever possible (when you need loops).
Move left to right.
A(i,j,k)loop order isdo for i/for j/for k

# Initializing Arrays in C++

* Arrays can be initialized when created
  * float A[3]={10.,20.,30.}
* Curly braces are required.
* Example
* #include <iostream>
* using namespacestd;
* intmain(intargc, char **argv){
  * constintn=5;
  * float A[n]={10.,20.,30.,40.,50.};
  * for (inti=0;i<n;i++){
    * cout<<A[i]<<" ";
  * }
  * cout<< "\n";
* return 0;}

# Initializing (Continued)

Elements not explicitly initialized will be set to 0.

Try it:

In the program on the previous page, try setting

float A[n]={};

Then try

float A[n]={10.,20.,30.};

with no other changes to the program

# WARNING WARNING WARNING

C++ happily lets you “walk off” your array.

Most commonly this occurs when you have variables and you end up attempting to access an element outside of the declared size.

This usually results in a segmentation violation or sometimes garbage results.

Example: in your previous code change

cout<< A[i]<<"";

To

cout<< A[i+1]<<" ";

# Multidimensional Arrays in C++

* Multidimensional arrays are just "arrays of arrays" in C++.
* They are declared with multiple brackets:
* float A[2][5];
* Elements are referenced like
* A[0][2]
* A[i][j]
* Initialize like
* A={{1.,2.,3.,4.,5.},
    * {6.,7.,8.,9.,10.}};

# C++ Array Properties

The arrays we have discussed are "C style arrays)

They are just blocks of memory with no added metadata.

1D arrays are contiguous in memory but higher-dimensional arrays need not be.

The name of the array is also a _pointer_ to the address in memory of the first (zeroth) element of the array.

Higher-dimensional arrays cannot be fully dynamically defined.

# Passing Arrays to Procedures

We can only pass the _pointer_ to the first element of the array.

A pointer is a variable that holds the memory location of another variable.

Array names are really pointers and in C were usually explicitly so.

Intheprocedure's argument listyoucan declareyour array withoneempty bracket. Forhigher-dimensional arrays only the first dimension can be empty; the others must be specified.
Higher-dimensional arrays generally are declared and passed as pointers, but their dimensions _must_ be passed in this case.

# Example

In main:

float a[100];

In the function

floatmyfunc(float a[],intlength)

Invoke the function with

myfunc(a, 100);

More about this when we get to functions.

# Allocation and the New Operator

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

