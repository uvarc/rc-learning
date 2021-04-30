---
title: "Arrays"
toc: true
type: docs
weight: 35

menu:
    fortran_introduction:
        name: Arrays
        weight: 35

---

# Terminology

A _scalar_ is a single item (real/float, integer, character/string, complex, etc.)

An _array_ contains data of the __same type__ with each scalar element addressed by _indexing_ into the array.

An array has one or more _dimensions_ .  The _bounds_ are the lowest and highest indexes.  The _rank_ is the number of dimensions.

Fortran arrays are similar toNumPyarrays and also carry metadata (shape, size, bounds, some other data) about themselves.

# Fortran Arrays

* Arrays must be declared by type and either by size or by some indication of the number of dimensions.
  * We will do variable dimensions later
* REAL, DIMENSION(100) :: A
* By default the index starts at 1.  However, it can start at any integer less than the upper bound:
* REAL, DIMENSION(-1:101,0:3) :: A0
* Arrays may have zero size.
* Maximum (standard) dimensions <=F2003 is 7. Increases to 15 in F2008.

# Orientation

* Array elements are _adjacent_ in memory (this is one of their advantages) and are arranged linearly no matter how many dimensions you declare. If you declare a 3x2 array the order in memory is
* (1,1), (2,1), (3,1), (1,2), (2,2), (3,2)
* “Orientation” refers to how the array is stored _in_  _memory_ , not to any mathematical properties.
* Fortran is _column-major_ oriented. Most other languages are _row-major_ oriented.
* Loop indices should reflect this whenever possible (when you need loops).
* Fortran: outermost first.  Go right to left.
  * A(i,j,k)loop order isdo k/do j/doi

# Array Elements and Slices

* Each element can be addressed by its index or indices, enclosed in _parentheses_ .
  * A(3)
  * X(i,j)
  * Remember, starts at 1 by default
* Slices (subarrays)
* REAL, DIMENSION(100)      :: A
* REAL, DIMENSION(12)       :: B
* INTEGER, DIMENSION(20,10) :: N
* INTEGER, DIMENSION(20)    :: C
* B=A(1:12)
* C=N(:,i)  !ithcolumn of N

# Array Initialization

  * Arrays can be initialized to the same quantity by an array operation
  * A=0.0 !equivalent to A(:)=0.0
  * For small arrays, an array constructor can be written.
  * I=[1,0,0,0]
  * The constructor can use a construct called an implieddo(which is somewhat similar to a Python list comprehension):
  * X=[(real(i),i=1,100)]

# Array Operations

* Most of the mathematical functions are _overloaded_ to accept array arguments. They operate on the array(s) _elementwise_ .
  * T=3.0
  * A=3.14159*I
* B=sin(A)
* C=A/B !Watch out for zero elements

# Array Intrinsics

Modern Fortran has many intrinsic functions that operate on arrays.

Arrayintrinsicscan be classified as inquiry, construction and manipulation, and transformation/reduction.

# Array Construction Intrinsics

reshape(source,shape[,pad][,order])

merge(array1,array2,mask)

pack(array,mask[,vector])

unpack(vector,mask,field)

spread(source,dim,ncopies)

# Array Inquiry	 Intrinsics

* allocated(array)
  * returns.true.or.false.
* lbound(array),ubound(array)
* shape(array)
  * returns a rank-one array containing the dimensions
* size(array,[dim])
  * returns the size (the total number of elements).  If the optional argumentdimis not present it returns the total number of elements; if dim is present it returns the number of elements on that dimension.

# Array Transformation Intrinsics

* transpose(matrix)
  * matrix must be square and rank 2
* dot_product(V1,V2)
  * both must be rank 1
* matmul(A,B)
  * A and B must conform, must return a rank-2 array even if it’s (1,1)
  * Note: depending on compiler and version, might be slow
* minloc(array [,mask]) !first one it finds
* minloc(array, dim [,mask])
* maxloc(as above)
* minloc/maxlocreturn a rank-1 array of _indices_ .

# Array Reduction Intrinsics

minval(A [,dim])

maxval(A [,dim])

all(mask [,dim])

any(mask [,dim])

count(mask)

product(A [,dim])

sum(A [,dim])

Example:Ahas shape(4,5,6)

sum(A,2)has shape(4,6)and elementssum(A(i,:,j))

->Same behavior as similarNumPybuilt-ins.

# Example

* Open a new file (you can call it arrays.f90) and type
* program arrays
* ! Array demo
* implicit none
  * real, dimension(100)   :: A
  * real, dimension(10,10) :: B
  * integer, dimension(2)  :: shaper=[10,10]
  * integer                ::i
  * A=[(0.1*real(i),i=1,100)]
  * B=reshape(A,shaper)
  * print *, B(1:3,1:4)
* end program

# Allocatable Arrays

Arrays may be sized at runtime by making them _allocatable_ .  They are declared with anallocatableattribute and a colon for each dimension.

REAL, ALLOCATABLE, DIMENSION(:) :: A, B

If any dimension isallocatable, all must be.

They must be allocated before they are used, so their size must be known at runtime.

ALLOCATE(A(NMAX),B(MMAX))

Check whether an array is allocated withallocated(A)

if (allocated(A)) then

do something

endif

# Exercise

* In arrays.f90
  * Print the size of the array A
  * Change the fourth element to 1.1
* Declare a new real array W of rank 2 (two dimensions) and make itallocatable.  Allocate the array to size 10x10 and set all values to 1.0 using an array operation.
* Use a nested loop to set each element of W(i,j) to the sum of its row and column indices. Fortran loops should run over indices from right to left, e.g.
  * do j=1,N
    * do i=1,M
      * A(i,j)=something
    * enddo
  * enddo

# Masking Array Operations

* Masks can be constructed and used to perform bulk operations on arrays.
* real, dimension(100):: x
* assign x somehow
* if ( all(x>=0.0) ) then
* y=sqrt(x)
* endif

# Advanced Array Indexing

Arrays can be addressed with arrays of integers (but notlogicals).

integer, dimension(1)           ::maxtemp

real, dimension(365)            :: temps

character(len=5),dimension(365) :: dates

maxtemp=maxloc(temps)

print *, "maximum temp was at ",dates(maxtemp)

# Conditionals with Arrays

Logical arrays can be assigned with conditionals derived from other arrays to construct masks.

logical, dimension(365) ::is_max

integer                 :: day

is_max=temps==maxval(temps)

do day=1,size(is_max)

if (is_max(day)) then

print *, dates(day)

endif

enddo

# Where

Where functions like a “vectorized”loop+conditional.

The clauses must be array assignments.

where ( A>=0.0 )

B=sqrt(A)

elsewhere

B=0.

end where

# Exercise

* In arrays.f90
  * Add a WHERE to set elements to W equal to the corresponding elements of B if the element of B is less than 7.

