---
title: "Pure and Elemental Functions"
toc: true
type: book
weight: 67

menu:
    fortran_introduction:
        parent: Subprograms
        weight: 67

---

# Pure and elemental procedures

# PURE Functions

Side effects should be avoided in functions.  Fortran offers subroutines to handle situations where changes to the parameters make sense.

The programmer can declare a functionPUREto tell the compiler it is free of side effects.

pure functionmyfunc(x)

integer          ::myfunc

real, intent(in) :: x

Pure functions must declare all parametersintent(in).

# PURE Procedures

* Subroutines may also bePURE.  They may change their parameters but should declare those as intent(out).
* PUREprocedures must have an interface.
* Any additional procedures they call must bePURE
* Neither pure functions nor pure subroutines are permitted to
  * Alter any accessible global variables (e.g. from contains)
  * Perform any IO
  * SAVE any variables
  * Contain any STOP statement

# ELEMENTAL Procedures

PUREprocedures were intended for automatic parallelization.  However, a particularly useful derivative is theELEMENTALprocedure.

ELEMENTALprocedures operate elementwise on arrays.

AllELEMENTALfunctions must obey the rules forPUREfunctions. Arguments must be scalars.

elemental function f2c(tempF)

real             :: f2c

real, intent(in) ::tempF

f2c=(tempF-32.)/1.8

end function f2c

# Using ELEMENTAL Procedures

real                  ::tempF,tempC

real, dimension(100)  ::tempFs,tempCs

real, dimension(10,10)::dataF,dataC

tempC= f2c(tempF)

tempCs= f2c(tempFs)

dataC= f2c(tempCs)

The elemental function can be called for any arrays as long as they conform.  Each element is modified by the function appropriately.  It can also be called as a normal scalar function.

# Exercise

Write an elemental function that accepts a single parameter as an angle in degrees and converts it to radians.

Write a program that computes the cosine of 0 to 90 degrees in increments of 5 degrees.  Print the output.  Do it with a loop, and then by creating a one-dimensional array of angles and passing it to the function.

