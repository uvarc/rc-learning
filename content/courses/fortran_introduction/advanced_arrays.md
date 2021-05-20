---
title: "Advanced Array Usage"
toc: true
type: book
weight: 42

menu:
    fortran_introduction:
        parent: Arrays
        weight: 42

---

## Array Initialization

Arrays can be initialized to the same quantity by an array operation:
```fortran
A=0.0 !equivalent to A(:)=0.0

```
For small arrays, an array constructor can be written.
```fortran
I=[1,0,0,0]
```
The constructor can use the implied do construct:
```fortran
X=[(real(i),i=1,100)]
```

## Array Slices
Subarrays, also known as _slices_, may be extracted using the colon operator.
```fortran
REAL, DIMENSION(100)      :: A
REAL, DIMENSION(12)       :: B
INTEGER, DIMENSION(20,10) :: N
INTEGER, DIMENSION(20)    :: C
   ! Assign values to A and N
   B=A(1:12)
   C=N(:,i)  !ith column of N
```

The upper bound of the range is always included. If the first bound is omitted, it starts from 1.  If the second bound is absent, the slice is extracted to the end of the range.  A single colon `:` represents the full range along a dimension.

## Allocatable Arrays

So far we have examined static arrays, whose size is fixed at compile time.
Arrays may be sized at runtime by making them _allocatable_ .  They are declared with an ALLOCATABLE` attribute and a colon for each dimension.
```fortran
REAL, ALLOCATABLE, DIMENSION(:)   :: A, B
REAL, ALLOCATABLE, DIMENSION(:,:) :: C
```
If any dimension is allocatable, all must be.

These arrays must be allocated before they are used, so their size must be known at runtime.  More than one array may be allocated in a single `ALLOCATE` statement.
```
ALLOCATE(A(NMAX),B(MMAX),C(NMAX,MMAX))
```
Check whether an array is allocated with the intrinsic `ALLOCATED(A)`
```
if (allocated(A)) then
   do something
else
   allocate(A(some_size))
endif
```
or if we do not need to take any action if A is allocated:
```fortran
if ( .not. allocated(A)) then
   allocate(A(some_size))
```

## Advanced Array Indexing

Arrays can be addressed with arrays of integers (but not logicals).
```
integer, dimension(1)           :: maxtemp
real, dimension(365)            :: temps
character(len=5),dimension(365) :: dates

maxtemp=maxloc(temps)
print *, "maximum temp was at ",dates(maxtemp)
```

## Conditionals with Arrays

Logical arrays can be assigned with conditionals derived from other arrays to construct masks.  The `maxval` intrinsic finds the (first) maximum value in an array.
```fortran
logical, dimension(365) ::is_max
integer                 :: day

   is_max=temps==maxval(temps)
   print *, 'Maximum temperature(s) were at'
   do day=1,size(is_max)
      if (is_max(day)) then
         write(*,advance='no'), dates(day)
      endif
   enddo
   write(*,*)
```

**Example**

Pulling the array indexing capabilities all together we have a complete program:

{{< code-download file="/courses/fortran_introduction/arrayinds.f90" lang="fortran" >}}

This code contains some features, such as string concatenation, that we will study later.

**Exercises**

1. Download the program above.  Add the code from the "Conditionals With Arrays" section appropriately.  Compare your output to the maxloc (which returns an integer array of the indices of the maximum value).

2. Make all arrays that should be the same size as `temps` allocatable, leaving temps static.  Allocate all to the size and shape of the `temps` array.  For convenience you may introduce an integer that represents the size of `temps`.  This way we can accommodate data for a leap year by changing just the size of `temps`.	

{{< spoiler text="Example Solution" >}}
{{< code file="courses/fortran_introduction/solns/arrayinds.f90" lang="fortran" >}}
{{< /spoiler >}}
