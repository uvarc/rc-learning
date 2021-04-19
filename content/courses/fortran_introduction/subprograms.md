---
title: "Subprograms"
toc: true
type: docs
weight: 60

menu:
    fortran_introduction:
        name: Subprograms
        weight: 60

---

# What is a Subprogram

* A subprogram is a self-contained (but not standalone) program unit.  It performs a specific task, usually by accepting _parameters_ and returning a result to the unit that invokes (calls) it.
* Subprograms are essential to good code practice.  Among other benefits, they are
  * Reusable.  They can be called anywhere the task is to be performed.
  * Easier to test and debug than a large, catch-all unit.
  * Effective at reducing errors such as cut and paste mistakes.

# Functions and Subroutines

Unlike most languages, Fortran makes a distinction between __functions__ and __subroutines__ .

Functions take any number (up to compiler limits) of arguments and return one item.  This item can be a compound type.

Functions must be declared to a type like variables. (Better: use an interface.)

Subroutines take any number of arguments (up to the compiler limit) and return any number of arguments.  All communication is through the argument list.

If they are in the same file as the calling unit, subprograms _follow_ the caller.

# INTENT

* In order to manage side effects, Fortran 90 introducedINTENTfor declaring subprogram parameters.
* <type> INTENT(<state>) ::param
* where _state_ can bein,out, orinout.
* <type> INTENT(in)      ::param
  * It is illegal to change a parameter declaredINTENT(in)
* <type> INTENT(out)     ::param
  * The compiler will warn if a parameter declaredINTENT(out)is never changed.
* <type> INTENT(inout)   ::param
  * INTENT(inout)means that the programmer intends to overwrite the parameter.  Thus the programmer makes clear that the side effect is desired or necessary.  Parameters passed down to other subprograms must beinout.
* Always useINTENTin your programs.

# Functions

The return value is indicated by assigning to the name of the function.

FUNCTIONmyfunc(param1,param2,param3,param4)

<type>             ::myfunc

<type>, INTENT(in) :: param1, param2

<type>, INTENT(in) :: param3, param4

statements

myfunc=whatever

return        !Optional unless premature

END FUNCTIONmyfunc

# Alternative Declaration

<type> FUNCTION    & !continuation due to PPT

myfunc(param1,param2,param3,param4)

<type>, INTENT(in) :: param1, param2

<type>, INTENT(in) :: param3, param4

statements

myfunc=whatever

return        !Optional unless premature

END FUNCTIONmyfunc

# Renaming the Result

Normally the function value is returned by assigning a value to the name of the function.

We can return it in a different variable with theRESULTclause.

function summit(x,y) result(s)

Especially used for recursive functions (it is required in this case until F2008).

When usingRESULTwe declare the type of the name of theRESULTrather than the name of the function.

# Example

FUNCTIONmyfunc(param1,param2)RESULT value

<type>             :: value

<type>, INTENT(in) :: param1, param2

<type>, INTENT(in) :: param3, param4

statements

value=whatever

return        !Optional unless premature

END FUNCTIONmyfunc

# Subroutines

SUBROUTINEmysub(param1,param2,param3)

<type> INTENT(in)    :: param1

<type> INTENT(out)   :: param2

<type> INTENT(inout) :: param3

statements

return       ! Optional unless premature

END SUBROUTINEmysub

# Invoking Functions and Subroutines

* Function
  * Invoke by its name
  * x=myfunc(z,w)
  * y=c*afunc(z,w)
  * A function is just like a variable except it cannot be an _lvalue_ (appear on the left-hand side of =)
* Subroutine
  * Use thecallkeyword
    * CALLmysub(x,y,z)

# Passing by Reference

* Fortran passes all parameters by _reference_ , meaning that the memory location holding the variable (or its starting position) is what is actually passed.
* This means that any argument can be changed by the subprogram and it will be changed in the caller as well.  This is a __side effect__ .
* Subroutines operate _entirely_ by side effects.
  * Sometimes this is not called a “side effect” when it is intentional, only when it is unintentional.

# Exercise

1. Write a function that computes Euclidean distance between pointsx1,y1andx2,y2.  Fortran has a built-insqrtintrinsic that you should use.

Write the main program to call this function for

x1=-1, y1=2, x2=3, y2=5

x1=11,y1=4, x2=7, y2=9

2. Given two pointsx1,y1andx2,y2,write a subroutine to determine which is closer to a third pointx3,y3.  It should pass back a message.  You can pass in the points and call the Euclidean distance function from the subroutine, or you can pass in the two distances.  (The former would be better programming but if you feel uncertain please go ahead and compute distances separately for now.)  Test with

x3=10, y3=5

You will need to declare the function name in the calling unit as if it were a variable, e.g.

realeu_dist

# Passing Arrays to Subprograms

* Arrays may be passed in one of three ways.
* Static
  * Dimensions are declared as fixed numbers in both calling unit andcallee.
* Automatic
  * Dimensions may be passed in the argument list
* Assumed-Shape
  * Only the rank is given, with an appropriate number of colons.

# Examples

real, dimension(100) :: A

call sub(A)

subroutine sub(A)

real, dimension(100) :: A  ! in sub

real, dimension(n) :: A

call sub(A,n)

subroutine sub(A,n)

real, dimension(n) :: A  ! in sub

integer            :: n

real, dimension(n) :: A

call sub(A)

subroutine sub(A)

real, dimension(:) :: A   ! insub

# Local Arrays

Arrays that are local to a subprogram may be sized using an integer passed to the subprogram

double precision functionmyfunc(A,n)

integer,                        intent(in) :: n                                      double precision, dimension(n), intent(in) :: A

double precision, dimension(n)             :: B

# Interfaces

If your subprogram is not in a module (to be covered later) you should provide anINTERFACE.

TheINTERFACEis equivalent to the _prototype_ of some other languages.

Interfaces enable the compiler to check that the _number_ and _type_ of the argument list in invocations agrees with the declared parameter list.

Interfaces arenonexecutableand should be placed with (or after) variable declarations.

# Syntax

* INTERFACE
  * functionmyfunc(x,y,z)
  * implicit none
  * real ::myfunc
  * real ::x,y
  * complex :: z
  * end functionmyfunc
* END INTERFACE

# More Interfaces

* INTERFACE
  * SUBROUTINEmysub(x,y,z)
  * usemymod
  * implicit none
  * <type> :: x
  * <type> ::y,z
  * END SUBROUTINEmysub
* END INTERFACE

# Interface Blocks

* Only one interface block is required per program unit.
* INTERFACE
  * functionmysub
  * declarations
  * end functionmysub
  * subroutine mysub1
  * declarations
  * end subroutine mysub1
  * subroutine mysub2
  * declarations
  * end subroutine mysub2
* END INTERFACE

# Exercise

Correct your previous exercise with Euclidean distance function and subroutine to use an interface.  Each calling unit must have an interface for every subprogram it calls.

The interface _replaces_ declarations of the type of the function name.

# Saving and Deallocating

* According to the standard, local variables in a procedure aredeallocatedupon exit from the procedure.
* Allocatablelocal arrays are automaticallydeallocated(a form of “garbage collection”)
* If you need some local variables to retain their value from one call to another, use the SAVE keyword
  * SAVE var1, var2, var3
  * SAVE
* With no variable list it saves all variables
* Allocatablelocal arrays cannot be saved.

# Passing a Subprogram Name

The name of a subprogram can be passed to another subprogram.

Example: a numerical-integration subroutine needs the function to be integrated.

subroutine trap(f,a,b)

wherefis a function.

The unit in which the subprogram receiving the name is called must have an interface for the subprogram to be passed.

# Optional and Keyword arguments

# Optional Arguments

Subroutines and functions may take optional arguments.   Such arguments need not be passed.  If they are passed, they take on the passed value. They are declared with theOPTIONALattribute.

subroutinemysub(x,y,z,w)

implicit none

real, intent(in)           ::x,y

real, intent(in), optional ::z,w

# Using Optional Arguments

The call to the previously-defined subroutine could be

callmysub(a,b)

in which case c and d would have no values and the subroutine would need to handle that situation appropriately.  The call could also be

callmysub(a,b,c)

or

callmysub(a,b,c,d)

depending on how many of the optional arguments needed to be passed.

# Keyword Arguments

Suppose it were desired to passdbut notcin the preceding subroutine.  Thecparameter can be skipped by using a _keyword_ argument; the optional argument is called as

dummy=actual

wheredummyis its name in the program unit where it is defined, and theactualargument is its name in the calling program unit.

Example:

callmysub(aa,bb,w=d)

Positional (non-optional) arguments must appear before any optional or keyword arguments.

# The PRESENT Intrinsic

ThePRESENT()intrinsic function tests whether a particular optional argument is present in the argument list of the caller.   If it is not present, defaults can be set or other action taken.

Example

IF (PRESENT(w)) then

dd=w

ELSE

dd=3.14

ENDIF

