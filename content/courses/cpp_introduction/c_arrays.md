---
title: "C-Style Arrays"
toc: true
type: book
weight: 42

menu:
    cpp_introduction:
        parent: C-Style Arrays
        weight: 42
---

One of the most common data structures, especially for scientific and numerical programming, is a group of variables all of the same type.  This is frequently called an _array_.

## Terminology

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

## Orientation

Elements of an array are arranged linearly in memory no matter how many dimensions you declare.  If you declare a 3x2 array the order in memory is
```no-highlight
(1,1), (1,2), (2,1), (2,2), (3,1), (3,2)
```
“Orientation” refers to how the array is stored _in_  _memory_ , not to any mathematical properties.
C++ and most other languages are _row-major_ oriented.  Some (Fortran,Matlab, R) are _column-major_ oriented.
Loop indices should reflect this whenever possible (when you need loops).
Move left to right.
A(i,j,k)loop order isdo for i/for j/for k

## Initializing Arrays in C++

Arrays can be initialized when created using a _constructor_:
```c++
   float A[3]={10.,20.,30.}
```
Curly braces are required.

Example:

{{< code-download file="/courses/cpp_introduction/codes/simple_array.cxx" lang="c++" >}}

Elements not explicitly initialized will be set to 0.
Try it: In the above program, change the initialization to
```c++
float A[n]={};
```
Allow it to print the output.  Then try
```c++
float A[n]={10.,20.,30.};
```
(i.e. setting only three out of five) with no other changes to the program.

**WARNING**

C++ happily lets you “walk off” your array.
Most commonly this occurs when you attempt to access an element outside of the declared size.  For instance, in C++ an index of -1 will never be legal, but for ordinary C-style arrays it will not check if your program attempts to access a value like `A[-1]`.  It is also commmon, especially in loops, to try to access an element beyond the last one.  Do not forget that array indices are 0-based, so the last element of an array of size N will be N-1.

An error of this type usually results in a _segmentation violation_, or sometimes garbage results.

Example: in your previous code change
```c++
cout<< A[i]<<"";
```
To
```c++
cout<< A[i+1]<<" ";
```

## Multidimensional Arrays in C++

Multidimensional arrays are just "arrays of arrays" in C++.
They are declared with multiple brackets:
```c++
float A[2][5];
```
Elements are referenced like
```c++
A[0][2]
A[i][j]
```
Small arrays can be initialized as follows:
```
A={{1.,2.,3.,4.,5.},{6.,7.,8.,9.,10.}};
```

