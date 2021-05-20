---
title: "Subprogram Arguments"
toc: true
type: book
weight: 63

menu:
    fortran_introduction:
        parent: Subprogram Arguments
        weight: 63

---

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

Example:
```
callmysub(aa,bb,w=d)
```
Positional (non-optional) arguments must appear before any optional or keyword arguments.

### The PRESENT Intrinsic

The PRESENT() intrinsic function tests whether a particular optional argument is present in the argument list of the caller.   If it is not present, defaults can be set or other action taken.

Example:
```fortran
IF (PRESENT(w)) THEN
   dd=w
ELSE
   dd=3.14
ENDIF
```

## Passing a Subprogram Name

The name of a subprogram can be passed to another subprogram.
Example: a numerical-integration subroutine needs the function to be integrated.
```
subroutine trap(f,a,b)
```
where f is a function.

The unit in which the subprogram receiving the name is called must have an interface for the subprogram to be passed.

