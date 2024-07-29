---
date: "2021-04-05"
title: "Array Intrinsics"
weight: 43
---

Modern Fortran has many intrinsic functions that operate on arrays.
Array intrinsics can be classified as inquiry, construction and manipulation, and transformation/reduction.

In documentation, square brackets around an argument indicate that it is _optional_.  The argument _dim_ refers to the dimension.  Many intrinsics can optionally work on the individual dimensions of an array.

This list is not exhaustive, and some optional arguments are omitted for some functions. For details about each intrinsic see a compiler reference, such as for [gfortran](https://gcc.gnu.org/onlinedocs/gcc-4.9.4/gfortran/Intrinsic-Procedures.html#Intrinsic-Procedures).

## Array Construction Intrinsics

These create new arrays from old. `PACK` and `UNPACK` can be used to "flatten" multidimensional arrays and to return a one-dimensional array to multidimensional rank.  This can be particularly useful for environments such as parallel programming with the Message Passing Interface.  

```fortran
! Convert an array from one shape to another (total size must match)
RESHAPE(SOURCE,SHAPE[,PAD][,ORDER])
! Combine two arrays of same shape and size according to MASK
!   Take from ARR1 where MASK is .true., ARR2 where it is .false.
MERGE(ARR1,ARR2,MASK)
PACK(ARRAY,MASK[,VECTOR])
UNPACK(VECTOR,MASK,FIELD)
! Take ARR and make NCOPIES of it along dimension DIM. Given source has
!   rank n, result has rank n+1
SPREAD(SOURCE,DIM,NCOPIES)
```
**Example**
```fortran
mask=A<0
merge(A,0,mask)
! for C=1, D=[1,2]
print *, spread(C, 1, 2)            ! "1 1"
print *, spread(D, 1, 2)            ! "1 1 2 2"
```

## Array Inquiry Intrinsics

```fortran
! Allocated or not. Returns .true. or .false.
ALLOCATED(ARRAY)  
! Upper and lower bounds
LBOUND(ARRAY)
UBOUND(ARRAY)
! Returns a rank-one array containing the dimensions
SHAPE(ARRAY)      
! Returns the size (the total number of elements).  
!  If the optional argument dim is not present it returns the total number of 
!  elements; if dim is present it returns the number of elements on that 
!  dimension.
SIZE(ARRAY,[DIM]) 
! Returns the rank (extension, but widely supported)
RANK(ARRAY)
```

## Array Transformation Intrinsics

```fortran
! matrix must be square and rank 2
TRANSPOSE(MATRIX)
!both V1 and V2 must be rank 1
DOT_PRODUCT(V1,V2)
! A and B must conform, must return a rank-2 array even if it’s (1,1)
! Note: depending on compiler and version, might be slow
MATMUL(A,B)
! Returns the location of the first minimum it finds in array
MINLOC(ARRAY [,MASK]) 
! Along a particular dimension
MINLOC(ARRAY, DIM [,MASK])
! Like minloc for max
MAXLOC(ARRAY [,MASK])
```
The MINLOC and MAXLOc intrinsics return a rank-1 array of _indices_ .

## Array Reduction Intrinsics

```fortran
! MIN/MAXVAL return the first-encountered min/max values, optionally along DIM
!  If DIM is absent, result is a scalar, otherwise an array of rank n-1
MINVAL(A [,DIM] [,MASK])
MAXVAL(A [,dim] [,MASK])
! For sum/product see example below
SUM(A [,DIM] [,MASK])
PRODUCT(A [,DIM] [,MASK])
```
**Example**
A has shape(4,5,6). Then
`SUM(A,2)` has shape (4,6) and elements `SUM(A(i,:,j))`.

Pay attention to how sum and product work when a dimension is specified.  It can be unintuitive.
{{< code file="/courses/fortran-introduction/codes/arraysum.f90" lang="fortran" >}}

## Masking Array Intrinsics

These determine truth or falsity according to the specified mask.  `ALL` and `ANY` return a scalar logical value if `dim` is absent; otherwise they return an array of `rank(mask)-1`.
The `COUNT` intrinsic returns the number of `.true.` elements in `mask`, 
optionally along `dim` dimension.

```fortran
ALL(MASK [,DIM])
ANY(MASK [,DIM])
COUNT(MASK)
```
**Example**
```fortran
print *, count(A)
if all(A>0) then 
   A=B
endif
if any(A<0) then
   A=0.
endif
```
