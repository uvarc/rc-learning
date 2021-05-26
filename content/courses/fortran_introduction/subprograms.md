---
title: "Subprograms"
toc: true
type: book
weight: 61

menu:
    fortran_introduction:
        parent: Subprograms
        weight: 61

---

A _subprogram_ is a self-contained, but not standalone, program unit.  It performs a specific task, usually by accepting _parameters_ and returning a result to the unit that invokes (calls) it.
Subprograms are essential to good coding practice.  Among other benefits, they are
  * Reusable.  They can be called anywhere the task is to be performed.
  * Easier to test and debug than a large, catch-all unit.
  * Effective at reducing errors such as cut and paste mistakes.

Other general names for subprograms are _routines_,_procedures_, and _methods_. The word "method" is generally reserved for procedures defined within an _object_, but it is not conceptionally different from any other subprogram. 

Subprograms must be invoked or _called_ in order for any of their code to be executed.  

## Functions and Subroutines

Unlike most languages, Fortran makes a distinction between __functions__ and __subroutines__ .

Functions take any number (up to compiler limits) of arguments and return one item.  This item can be a compound type.
Functions must be declared to a type like variables, or must be defined in an _interface_ (prototype). 

Subroutines take any number of arguments, up to the compiler limit, and return any number of arguments.  All communication is through the argument list.
If they are in the same file as the calling unit, subprograms _follow_ the caller.

Variables in the argument list are often called _dummy arguments_ since they stand for _actual arguments_ that are defined in the calling unit.
Like any variable, they must be declared explicitly.  In Fortran this is done on separate lines, like for PROGRAM.

### Subroutines

The subroutine unit begins with its name and parameter list
```fortran
SUBROUTINE mysub(param1,param2,param3)
<type>  :: param1, param1, param3
```
It must terminate with the END statement.  
```
END [SUBROUTINE] [NAME]
```
The form END SUBROUTINE is highly recommended.  This can be followed by the name of the subroutine
```fortran
END SUBROUTINE mysub
```
but sometimes this leads to cut and paste errors.
A RETURN statement is optional unless a premature return is desired.
Invoking RETURN causes an _immediate_ return of control to the caller.  No other statements in the subprogram will be executed.

A subroutine is invoked through the CALL command
```fortran
CALL mysub(var1, var2, var3)
```

Note that the names of the actual and dummy arguments need not match, but they _must_ agree in number and type.

### Functions

Functions are declared in a manner similar to subroutines, but unlike subroutines they have a _type_.  The type of the function is the type of its return value.
```fortran
<type> FUNCTION myfunc(param1,param2,param3)
<type>   :: param1, param2, param3
```
A function receives its return value by assigning an expression to its name.
```fortran
myfunc=param1*param2/param3
```
As for subroutines, a RETURN statement is optional except for returning control due to some conditional.
They must also terminate with END
```
END [FUNCTION] [NAME]
```

Functions are invoked by name.  They can enter into an expression anywhere on the right-hand side of the assignment operator (=).
```fortran
z=4.*myfunc(var1,var2,var3)
```
As for subroutines, the names of the actual arguments need not be the same as those of the dummies, but the number and type must match.

Because functions have a type, they must be _declared_ like a variable in any program unit that invokes them.  Better yet, use an [interface](courses/fortran_introduction/subprogram_args).

Subroutines have no return type and cannot be declared.

Example:
{{< code-download file="/courses/fortran_introduction/subprogs.f90" lang="fortran" >}}

#### Renaming Function Results 

Normally the function value is returned by assigning a value to the name of the function.
We can return it in a different variable with the RESULT clause.
```fortran
FUNCTION summit(x,y) RESULT(s)
```
This is especially useful for recursive functions; it is required in this case until the F2008 standard, and not all compilers support F2008 in full yet.
When using RESULT we declare the type of the name of the RESULT rather than the name of the function.  The caller must still declare the function, however (or use an [interface](/courses/fortran_introduction/interfaces).

Example
```fortran
FUNCTION myfunc(param1,param2) RESULT value
<type>             :: value
<type>, INTENT(in) :: param1, param2
<type>, INTENT(in) :: param3, param4

statements

value=whatever
return        !Optional unless premature
```
