---
title: "Array Intrinsics"
toc: true
type: docs
weight: 43

menu:
    fortran_introduction:
        name: Array Intrinsics
        weight: 43

---

Modern Fortran has many intrinsic functions that operate on arrays.
Array intrinsics can be classified as inquiry, construction and manipulation, and transformation/reduction.

In documentation, square brackets around an argument indicate that it is _optional_.  The argument _dim_ refers to the dimension.  Many intrinsics can optionally work on the individual dimensions of an array.

This list is not exhaustive, and some optional arguments are omitted for some functions. For details about each intrinsic see a compiler reference, such as for [gfortran](https://gcc.gnu.org/onlinedocs/gcc-4.9.4/gfortran/Intrinsic-Procedures.html#Intrinsic-Procedures).

## Array Construction Intrinsics

These create new arrays from old. `PACK` and `UNPACK` can be used to "flatten" multidimensional arrays and to return a one-dimensional array to multidimensional rank.  This can be particularly useful for environments such as parallel programming with the Message Passing Interface.

```fortran
reshape(source,shape[,pad][,order])
merge(array1,array2,mask)
pack(array,mask[,vector])
unpack(vector,mask,field)
spread(source,dim,ncopies)
```
Example:
```fortran
mask=A<0
merge(A,0,mask)
```

## Array Inquiry Intrinsics

```fortran
! Allocated or not. Returns .true. or .false.
allocated(array)  
! Upper and lower bounds
lbound(array),ubound(array)
! Returns a rank-one array containing the dimensions
shape(array)      
! Returns the size (the total number of elements).  
!  If the optional argument dim is not present it returns the total number of 
!  elements; if dim is present it returns the number of elements on that 
!  dimension.
size(array,[dim]) 
! Returns the rank (extension, but widely supported)
rank(array)
```

## Array Transformation Intrinsics

```fortran
! matrix must be square and rank 2
transpose(matrix)
!both V1 and V2 must be rank 1
dot_product(V1,V2)
!A and B must conform, must return a rank-2 array even if it’s (1,1)
! Note: depending on compiler and version, might be slow
matmul(A,B)
! Returns the location of the first minimum it finds in array
minloc(array [,mask]) 
! Along a particular dimension
minloc(array, dim [,mask])
! Like minloc for max
maxloc(array [,mask])
```
The minloc and maxloc intrinsics return a rank-1 array of _indices_ .

## Array Reduction Intrinsics

```fortran
minval(A [,dim])
maxval(A [,dim])
product(A [,dim])
sum(A [,dim] [,mask])
product(A [,dim] [,mask])
```
Example:
A has shape(4,5,6). Then
`sum(A,2)` has shape (4,6) and elements `sum(A(i,:,j))`.

Pay attention to how sum and product work when a dimension is specified.  It can be nonintuitive.
{{< code file="/courses/fortran_introduction/arraysum.f90" lang="fortran" >}}

## Masking Array Intrinsics

These determine truth or falsity according to the specified mask.  `ALL` and `ANY` return a scalar logical value if `dim` is absent; otherwise they return an array of `rank(mask)-1`.
The `COUNT` intrinsic returns the number of `.true.` elements in `mask`, 
optionally along `dim` dimension.

```fortran
all(mask [,dim])
any(mask [,dim])
count(mask)
```
Example:
```fortran
print *, count(A)
if all(A>0) then 
   A=B
endif
if any(A<0) then
   A=0.
endif
```
