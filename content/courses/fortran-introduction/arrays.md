---
title: "Introduction to Arrays"
date : "2021-04-5T00:00:00"
toc: true
type: book
weight: 41

menu:
    fortran-introduction:
        parent: Arrays
        weight: 41

---

The arrays is one of the most important types in Fortran. It can represent many mathematical entities such as grids, matrices, vectors, and so forth.
The members of an array are frequently called _elements_.
From the computational point of view, an  _array_ is a data structure that contains data of the __same type__ with each scalar element addressed by _indexing_ into the array.  Indices must be integers.
An array has one or more _dimensions_ .  The _bounds_ are the lowest and highest indexes.  The _rank_ is the number of dimensions.  The _size_ of an array is the number of elements.  The _shape_ is a tuple giving the size in each individual dimension.

## Fortran Arrays

Arrays must be declared by type and either by size or by some indication of the number of dimensions.  The simplest case is static arrays, which are declared to be of a fixed size and shape.
```fortran
REAL, DIMENSION(100)            :: A
INTEGER, DIMENSION(50,50)       :: INDS
CHARACTER(LEN=3), DIMENSION(12) :: MONTHS
```
By default the index starts at 1.  However, it can start at any integer less than the upper bound:
```fortran
REAL, DIMENSION(-1:101,0:3) :: A0
```
Arrays may have zero size.
The maximum (standard) dimensions up to the F2003 standard is 7. This increases to 15 in F2008.
Fortran arrays carry metadata (shape, size, bounds, some other data) about themselves and these data can be extracted by intrinsic functions.

Each element can be addressed by its index or indices, which must be enclosed in _parentheses_.
```fortran
A(3)
X(i,j)
```
Remember that the bounds start at 1 by default.

### Compiler Bounds Checking

One of the _most_ common errors in programming, in all languages that support an array or something like it, is attempting to access an element with values for the indices that are outside the declared bounds.  This frequently happens with loops such as
```fortran
! Average nearest neighbors
do j=1,m
   do i=1,n
      A(i,j)=0.25*(B(i+1,j)+B(i,j+1)+B(i-1,j)+B(i,j-1))  !OOPS
   enddo
enddo
```
Most usually this results in an error such as "Segmentation fault."
In Fortran, compilers know the size and shape of each array, once allocated, so they are able to check each operation to make sure it is within the bounds.  However, since this can _greatly_ slow down your executable, it must be invoked at compile time.  The name of the option may vary from one compiler to another.  
For gfortran and Intel they are:
```
gfortran -g -fbounds-check mycode.f90
ifort -g -CB mycode.f90
ifort -g -check bounds mycode.f90 
```
Be sure to remove the options `-g -<array check>` and replace with `-O` once your program is debugged.

## Array Operations

Fortran supports _array operations_ in which an action is applied across an entire array, elementwise.  All arithmetic operators and most of the mathematical functions are _overloaded_ to accept array arguments, for arrays of numerical type.
```fortran
T=3.0
A=3.14159*I
B=sin(A)
C=A/B !Watch out for elements that are zero
```

## Array Orientation

Array elements are _adjacent_ in memory and are arranged linearly no matter how many dimensions you declare. If you declare a 3x2 array the order in memory is
```
(1,1), (2,1), (3,1), (1,2), (2,2), (3,2)
```
“Orientation” refers to how the array is stored _in_  _memory_ , not to any mathematical properties.
Fortran is _column-major_ oriented. Most other languages are _row-major_ oriented.
Loop indices should reflect this whenever possible (when you need loops).
Fortran: outermost first.  Go right to left.
```fortran
    do k=1,nz-1
       do j=1,ny-1
          do i=1,nx
              A(i,j,k) = C(i,j,k) !loop order is do k/do j/do i
          enddo
       enddo
    enddo
```
Array operations can be used to avoid loops in many cases, and the compiler will be able to optimize them automatically.
```fortran
A=C
```

## Obtaining Size and Shape Information

The `SIZE(ARRAY [,DIM])` function returns the number of elements in the array if `DIM` is absent.  If it is present, it returns the number of elements in that dimension, e.g. number of rows or columns.

The `SHAPE(ARRAY)` function returns a one-dimensional array whose elements are the dimensions of the array.

The `RANK(ARRAY)` function returns the rank (number of dimensions).

## Where

Where functions like a “vectorized”loop+conditional.
The clauses must be array assignments.
```fortran
where ( A>=0.0 )
   B=sqrt(A)
elsewhere
   B=0.
end where
```

**Exercises**

* Open a new file (you can call it arrays.f90) and type
```
program arrays
! Array demo
```fortran
implicit none
   real, dimension(10)    :: A
   integer                :: i, j
      A=[(0.2*real(i),i=1,10)]
end program
```
* In arrays.f90
1 Print the size and shape of the array A
2 Declare a new real array W of shape 10x10. Set all values to 1.0 using an array operation. 
3 Declare another 10x10 array Z.  Use a nested loop to set each element of W(i,j) to the sum of its row and column indices. Remember that Fortran loops should run over indices from right to left, e.g.
```fortran
do j=1,N
   do i=1,M
      A(i,j)=something
   enddo
enddo
```
4 Change the fourth column of the second row of Z to 1.1

* In arrays.f90
  * Add a WHERE to set elements to W equal to the corresponding elements of Z if the element of Z is less than 7.  Print W(2,4).

{{< spoiler text="Example solution" >}}
{{< code file="/courses/fortran-introduction/solns/arrays.f90" lang="fortran" >}}
{{< /spoiler >}}

