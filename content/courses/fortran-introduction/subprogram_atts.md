---
date: "2021-04-05"
title: "Subprogram Attributes"
weight: 65
---

Subprograms can be defined with attributes for particular conditions or behaviors.  They can call themselves, or operate elementwise on arrays.

## Pure and Elemental Procedures

### PURE Functions

Side effects should be avoided in functions.  Fortran offers subroutines to handle situations where changes to the parameters make sense.
The programmer can declare a function PURE to tell the compiler it is free of side effects.
Pure functions must declare all parameters INTENT(IN).

Subroutines may also be PURE.  They may change their parameters but must declare those as INTENT(OUT).  INTENT(INOUT) is not permitted for PURE subroutines.

There are strict rules for PURE procedures.
* PURE procedures must have an interface.
* Any additional procedures they call must also be PURE.
* Neither PURE functions nor PURE subroutines are permitted to
  * Alter any accessible global variables (e.g. from CONTAINS);
  * Perform any IO -- this can make debugging inconvenient;
  * SAVE any variables;
  * Contain any STOP statement.  RETURN is permitted.

```fortran
PURE FUNCTION myfunc(x)
INTEGER          ::myfunc
REAL, INTENT(IN) :: x
```

Since there is PURE, there is also IMPURE, so that the programmer is explicit about the presence of side effects.

### ELEMENTAL Procedures

PURE procedures were intended for automatic parallelization.  However, a particularly useful derivative is the ELEMENTAL procedure.
ELEMENTAL procedures operate _elementwise_ on arrays.
All ELEMENTAL procedures must obey the rules for PURE procedures. In addition, all arguments must be scalars.
```fortran
ELEMENTAL FUNCTION f2c(tempF)
REAL             :: f2c
REAL, INTENT(IN) ::tempF

   f2c=(tempF-32.)/1.8

END FUNCTION
```

#### Using ELEMENTAL Procedures
An elemental procedure can be called for any arrays as long as they conform to the requirements for all operations in the procedure.  Each array element is modified by the procedure appropriately.  The procedure can also be called as a normal scalar function.

```fortran
PROGRAM elements
REAL                  ::tempF,tempC
REAL, DIMENSION(100)  ::tempFs,tempCs
REAL, DIMENSION(10,10)::dataF,dataC

INTERFACE
   ELEMENTAL FUNCTION f2c(tempF)
     REAL             :: f2c
     REAL, INTENT(IN) ::tempF
   END FUNCTION
END INTERFACE

! Set up tempFs somehow
tempC= f2c(tempF)
tempCs= f2c(tempFs)
dataC= f2c(dataF)
END PROGRAM
```

Like PURE procedures, ELEMENTAL procedures must have an interface.

Starting with Fortran 2008, it was recognized that the restrictions of PURE were sometimes unnecessary and detrimental to developing ELEMENTAL procedures, which have many uses for which PURE is irrelevant.  They may thus be declared
```
IMPURE ELEMENTAL myfunc(x)
```
Absent IMPURE, the procedure must obey the rules for PURE.

### RECURSIVE Procedures

Recursive procedures call themselves.  This may seem impossible, but the compiler sets up multiple copies, usually in a section of memory called the _stack_.  Without some care in implementing the algorithm, recursion can lead to stack overflow.  Even worse, a recursive procedure _must_ have a stopping condition or the result is infinite recursion, at least until the stack overflows and the executable or even the whole computer crashes.

Most recursive algorithms have an iterative (while loop) equivalent, which may perform better, but the recursive algorithm may be simpler and easier to understand.

Both functions and subroutines can be RECURSIVE.  Up to the F2008 standard, recursive functions require a RESULT clause.  Recursive subroutines do not support RESULT and do not require it. Starting with the F2018 standard, the default is for subprograms to be assumed RECURSIVE, so the keyword will no longer be required unless a compiler option is used to change the default behavior.  Another keyword NON_RECURSIVE can be used to make a subprogram explicitly iterative.

One of the most famous examples of a recursive algorithm is the Fibonacci sequence.  To compute the Nth number in the sequence, we can use

$$ F_0 = 0 $$
$$ F_1 = 1 $$
$$ F_{N}=F_{N-1}+F_{N-2} $$

{{< code-download file="/courses/fortran-introduction/codes/fibonnaci.f90" lang="fortran" >}}

**Exercise**

1. Write an elemental function that accepts a single parameter as an angle in degrees and converts it to radians.

2. Write a program that computes the radian equivalent of 0 to 90 degrees in increments of 5 degrees.  Print the output.  Do it with a loop, and then by creating a one-dimensional array of angles and passing it to the function.

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/fortran-introduction/solns/deg_rad.f90" lang="fortran" >}}
{{< /spoiler >}}

