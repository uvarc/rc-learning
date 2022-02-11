---
title: "Array Arguments"
toc: true
type: book
weight: 64

menu:
    fortran_introduction:
        parent: Array Arguments
        weight: 64

---

Passing scalar arguments is straightforward, but many, perhaps most, Fortran programs are oriented around arrays and must pass them to and from subprograms.
Since Fortran passes _all_ variables by reference, i.e. by passing a pointer, there is no special behavior compared to scalars, unlike many other languages.

## Passing Arrays to Subprograms

* Arrays may be passed in one of three ways.
* Static
  * Dimensions are declared as fixed numbers in both calling unit and callee.
* Automatic
  * Dimensions may be passed in the argument list
* Assumed-Shape
  * Only the rank is given, with an appropriate number of colons.
  * Assumed-shape arrays require an interface.

**Examples**
We will show examples for subroutines but the rules are the same for functions.
Static:
```fortran
real, dimension(100) :: A

call sub(A)

subroutine sub(A)
real, dimension(100) :: A  ! in sub
```
A variable may be used but it must be declared PARAMETER.  Some compilers, specifically Intel's, may require that the parameter be declared before any arrays are declared that use it.
```fortran
integer, parameter  :: N=100
real, dimension(N)  :: A
real, dimension(N,N):: B
```

Automatic:
```fortran
real, dimension(100) :: A

call sub(A,n)
subroutine sub(A,n)
real, dimension(n) :: A  ! in sub
```
Assumed-shape:
```fortran
integer,parameter  :: n=100
real, dimension(n) :: A

call sub(A)

subroutine sub(A)
real, dimension(:) :: A   ! in sub
```

Though the dimensions need not be known for assumed-shape arrays, the rank must match.

**Example**
{{< code-download file="courses/fortran_introduction/codes/pass_arrays.f90" lang="fortran" >}}

### Allocating Arrays in a Subprogram

You may allocate an array inside a subprogram and return it.  An interface is required.  The array must be declared `allocatable` throughout the chain of calling units and must be `intent(inout)`.  

In the example below, no value is assigned initially. Most compilers will initialize the array to zero, but this is not guaranteed.

**Exercise**
Run the program as is. Correct the lack of initialization in the subroutine.

{{< code-download file="/courses/fortran_introduction/codes/pass_alloc.f90" lang="fortran" >}}

### Local Arrays in Subprograms

Arrays that are _local_ (not in the parameter list) to a subprogram may be sized using an integer passed to the subprogram.

```fortran
double precision function myfunc(A,n)
integer,                        intent(in) :: n
double precision, dimension(n), intent(in) :: A
double precision, dimension(n)             :: B

  do things with A and B
  myfunc=whatever
end function
```
