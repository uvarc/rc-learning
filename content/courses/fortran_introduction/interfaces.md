---
title: "Interfaces"
toc: true
type: book
weight: 62

menu:
    fortran_introduction:
        parent: Interfaces
        weight: 62

---

If your subprogram is not in a [module](/courses/fortran_introduction/modules) you should provide an INTERFACE.
The INTERFACE is equivalent to the _prototype_ of some other languages.
Interfaces enable the compiler to check that the _number_ and _type_ of the argument list in invocations agrees with the declared parameter list.
Interfaces are nonexecutable and should be placed with (or immediately following) variable declarations.

## Syntax

Function
```fortran
INTERFACE
  FUNCTION myfunc(x,y,z)
     implicit none
     real ::myfunc
     real ::x,y
     complex :: z
  END FUNCTION
END INTERFACE
```
Subroutine
```fortran
INTERFACE
  SUBROUTINE mysub(x,y,z)
     use precisions
     use mymod
     implicit none
     real    :: x
     real_dp ::y,z
  END SUBROUTINE mysub
END INTERFACE
```

The simplest way to set up an interface is to copy the first lines of the subprogram.  All statements that may affect the ability of the compiler to check number and type of the arguments must be included.  This encompasses USE (for modules), IMPLICIT, and all declarations of the arguments.  Declarations for local variables are _not_ needed and should not be included.  The END statements must include the FUNCTION or SUBROUTINE keyword as appropriate. 

The interface terminates with END INTERFACE.

## Interface Blocks

Only one interface block is required per program unit.
```fortran
INTERFACE
  function mysub
    declarations
  end function
  subroutine mysub1
    declarations
  end subroutine
  subroutine mysub2
    declarations
  end subroutine
END INTERFACE

# Exercise

Correct your previous exercise with Euclidean distance function and subroutine to use an interface.  Each calling unit must have an interface for every subprogram it calls.

The interface _replaces_ declarations of the type of the function name.

