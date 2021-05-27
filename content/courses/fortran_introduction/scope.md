---
title: "Variable Scope"
toc: true
type: book
weight: 66

menu:
    fortran_introduction:
        parent: Variable Scope
        weight: 66

---

**Scope** is the range over which a particular variable is defined and can have a value.  In Fortran, scope is defined by the _program unit_.
A calling unit may have a variable named `x`, and a function may also have a variable named `x`, and if `x` is not an argument to the function then it will be distinct from the `x` in the calling unit.
```fortran
x=20.
call sub(x)

etc.

subroutine sub(y)
real, intent(inout) :: y
real                :: x

x=10.
y=30.

end subroutine sub
```

An interface is also a scoping unit.
```fortran
program 
   implicit none
   real   :: x
   interface
      function myfunc(x)
      real, intent(in) :: x
      end function
   end interface
end program
```
In the above example, the `x` in the interface is not connected to the `x` in the interface.

A variable that is only in scope in a particular unit is said to be _local_ to that unit.  Variables that are visible from more than one unit are _global_ to those units.

## BLOCK 

The Fortran 2008 standard introduced the BLOCK construct.  A BLOCK is a scoping unit that is finer-grained than the program-unit level.  Variables may be declared within a BLOCK. Variables declared within the block are local to it.  Variables declared outside the block are global to it.  

IMPLICIT statements cannot appear in a block, but they do affect variables within the block.

{{< code file="/courses/fortran_introduction/codes/blocker.f90" lang="fortran" >}}

## CONTAINS and Nested Procedures

CONTAINS is a way to nest procedures within another unit.
The CONTAINS keyword extends the scope into the contained program unit.
The end of the “container” must _follow_ the end of the “containee”
A contained subprogram can access _all_ the variables in the container except those that are explicitly passed.
The interface is implicit and should not be made explicit.
Only one level of nesting is permitted.

Example
```
PROGRAM myprog
IMPLICIT NONE
   REAL  ::x,y,z

   x=5.; y=10.
   CALL mysub(z)

   CONTAINS

      SUBROUTINE mysub(w)
         REAL, INTENT(INOUT) :: w
            w=x+y
      END SUBROUTINE

END PROGRAMm
```

## COMMON and INCLUDE

COMMON is a deprecated feature that is frequently seen in older code.  It is a means of providing global variables.  Global variables are variables that are in scope in at least the entire file in which they are declared; they are a frequent source of bugs since they may result in a "memory" that should not be present. 
```fortran
common /comname/ var1, var2, var3
```
The variables in the common list will be available to _any_ program unit that includes the above line.  Variables in common between two program units should **not** be passed as subroutine parameters.
Newer code should use [modules](/courses/fortran_introduction/modules) rather than COMMON.

### Pitfalls with Common

The COMMON statement _must_ be in every program unit that will share the variables, and it _must_ be identical in each one.  It is highly recommended that any code using common put each one into a separate file and use the INCLUDE statement to merge the files. INCLUDEis a standard Fortran statement, not a preprocessor statement; its syntax is
```fortran
include 'file.h'
```
where `file.h` can be any name; Fortran does not have a rule about file extensions for included files.

COMMON is a frequent source of memory errors.
COMMON makes interface control difficult to impossible.

The recommended first step in updating [old code](/courses/fortran_introduction/updating_old_code) is to replace all COMMON with modules and then gradually to move the variables into the appropriate parameter lists. 

COMMON is a vestige of the early computer era, when main memory was measuring in megabytes or even kilobytes, and every byte was precious.  With COMMON the global variables would occupy a single spot in memory, would not be copied, and could be reused.  In particularly old code it is not unusual for arrays in COMMON to be reshaped implicitly, since COMMON inherently represents a large linear block of memory.  Sometimes the EQUIVALENCE statement is still seen, by which types could be converted implicitly as long as they occupied the same number of bytes.
