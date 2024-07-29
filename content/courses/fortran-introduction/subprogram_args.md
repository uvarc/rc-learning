---
date: "2021-04-05"
title: "Subprogram Arguments"
weight: 63
---

## Pass by Reference and INTENT

Unlike most languages, Fortran passes all arguments by _reference_.  This effectively means that what is actually passed is the memory location of the argument.  Consequently, any change to the argument in the subprogram, intended or not, will change the variable outside as well. Changing the value of an argument is called a _side effect_.  Side effects can be legitimate -- subroutines rely on them -- but they should be controlled.  For this reason, Fortran introduced the `INTENT` attribute for subprogram parameters.
```fortran
INTENT(IN)   ! Changing the variable in the subprogram throws a fatal error
INTENT(OUT)  ! Not changing the variable in the subprogram throws a fatal error
INTENT(INOUT)! Indicates that the programmer intends to overwrite the variable
```

**Example**
```
subroutine mysub(x,y,z)
   real, intent(in)    :: x
   real, intent(out)   :: y
   real, intent(inout) :: z
      y=x-z
      z=y+x
end subroutine
```
As a general rule, all arguments to a FUNCTION should be INTENT(IN).

## Saving and Deallocating Subprogram Arguments

According to the standard, the memory used by local variables in a subprogram is freed upon exit from the procedure.
Allocatable local arrays are automatically deallocated (this is a form of “garbage collection”).
If you need some local variables to retain their value from one call to another, use the SAVE keyword
```
SAVE var1, var2, var3
SAVE
```
With no variable list it saves all local variables.
Note that allocatable local arrays cannot be SAVEd.

Many compilers do not actually free the memory of non-allocatable local variables and some old programs rely on this behavior.  Compilers have an option to ensure that all local variables are saved.
```
gfortran -fno-automatic mycode.f90
ifort -save mycode.f90
```

## Optional and Keyword Arguments

### Optional Arguments

Subroutines and functions may take optional arguments.   Such arguments need not be passed.  If they are passed, they take on the passed value. They are declared with the OPTIONAL attribute.
```fortran
subroutine mysub(x,y,z,w)
implicit none
real, intent(in)           ::x,y
real, intent(in), optional ::z,w
```

The call to the previously-defined subroutine could be
```fortran
callmysub(a,b)
```
in which case c and d would have no values and the subroutine would need to handle that situation appropriately.  The call could also be
```fortran
callmysub(a,b,c)
```
or
```fortran
callmysub(a,b,c,d)
```
depending on how many of the optional arguments needed to be passed.

### Keyword Arguments

Suppose it were desired to pass `d` but not `c` in the preceding subroutine.  The `c` parameter can be skipped by using a _keyword_ argument; the optional argument is called as
```
dummy=actual
```
where `dummy` is its name in the program unit where it is defined, and the `actual` argument is its name in the calling program unit.

**Example**
```
callmysub(aa,bb,w=d)
```
Positional (non-optional) arguments must appear before any optional or keyword arguments.

### The PRESENT Intrinsic

The PRESENT() intrinsic function tests whether a particular optional argument is present in the argument list of the caller.   If it is not present, defaults can be set or other action taken.

**Example**
```fortran
IF (PRESENT(w)) THEN
   dd=w
ELSE
   dd=3.14
ENDIF
```

## Passing Character Variables

Characters declared with a fixed length may be passed to a subprogram using a dummy length.
```fortran
character(len=20) :: str
   call mysub(str)
end program
subroutine mysub(str)
   implicit none
   character(len=*), intent(in) :: str
end subroutine
```

## Passing a Subprogram Name

The name of a subprogram can be passed to another subprogram.
**Example**
a numerical-integration subroutine needs to be able to call the function to be integrated.
```
subroutine trap(f,a,b,h,n)
```
where f is a function.

The procedure name to be passed must have an interface in the unit that invokes the subprogram that will take this parameter.  The subprogram must also have an interface for the passed procedure. The older syntax `EXTERNAL func` may be used for the parameter, but new code should use `INTERFACE`.  This will result in an INTERFACE block within another INTERFACE, which may look a bit wordy but is the modern way to declare this type of parameter.
```fortran
interface
  real function trap(f,a,b,h,n)
      implicit none
      real,    intent(in)   :: a, b, h
      integer, intent(in)   :: n
      interface
         real function f(x)
         implicit none
         real, intent(in) :: x
         end function
       end interface
   end function
   real function f(x)
        implicit none
        real, intent(in) :: x
   end function
end interface
```

{{< spoiler text="Full example of passing a subprogram as a dummy variable" >}}
{{< code-download file="/courses/fortran-introduction/codes/trap.f90" lang="fortran" >}}
{{< /spoiler >}}

**Exercise**

Write a program for comparing Euclidean distances.  The program should implement a function that takes the coordinates of two points and returns their distance.  Implement a subroutine that invokes this function for three points to determine which of the first two is closer.  Use interfaces.
Each calling unit must have an interface for every subprogram it calls. 

Use intent and implicit none.  Remember that implicit none must be declared in each unit.
You may use arrays to represent the points.

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/fortran-introduction/solns/euclid.f90" lang="fortran" >}}
{{< /spoiler >}}
