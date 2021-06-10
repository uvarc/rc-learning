---
title: "Loops"
toc: true
type: book
weight: 32

menu:
    fortran_introduction:
        parent: Basic Programming Constructs
        weight: 32
---

Much computing is repetetive work.  Evaluate an expression many times with different values.  Read lines of a file.  Update a large number of variables.  To accomplish this, we use _loops_.

## DO Loop

`DO` is the equivalent of `for` in languages like C/C++.
DO executes a fixed number of iterations unless explicitly terminated.
DO can iterate only over integer sequences.  Some compilers support real indices as an extension, but they should be avoided.
```fortran
INTEGER   :: L, U, S
INTEGER   :: I

DO I=L,U,S
   code
END DO
```
`I` is the _loop variable_,
`L` is the _lower bound_,
`U` is the _upper bound_, and
`S` is the stride.  It is equal to 1 if not present.
The stride can be negative, in which case `L` must be greater than `U`.
Fortran always starts at the first bound and includes the second bound.

QUIZ
The standard requires that loop variables be integers.  How would I implement loop variables that are real?
<br>
How might real loop variables be a problem?

### Implied DO

The _implied do_ is used in a few circumstances, specifically input/output and array construction.
```
(var(iterator),iterator=lbound,ubound,s)
```
The parentheses are required.

Example:
```fortran
(a(i),i=1,20)
```
Implied do loops can be nested.
```fortran
((r(i,j),j=1,M),i=1,N)
```

## WHILE Loops

Whereas `DO` loops execute a particular number of iterations, `WHILE` loops iterate based on the truth or falsity of an expression.  The `WHILE` continues as long as the expression is `.true.` and terminates when it becomes `.false.`. It is up to the programmer to be sure to add statements to ensure that the expression eventually evaluates to `.false.` so the loop will end.
```fortran
DO WHILE (<logical expression>)
   statement
   statement
   statement somewhere to check expression
END DO
```

# Example
```fortran
program demo
implicit none
    integer  :: x, y, z
    x=-20
    y=-10
    do while (x<0 .and. y<0)
        x=10-y
        y=y+1
        z=0
    enddo
    z=1
    print *, x, y, z
end program
```

## Exiting Early and Skipping Statements

The `EXIT` statement leaves the loop immediately.
`EXIT` is able to break out of _only_ the loop level _in which it appears_.  It cannot break from an inner loop all the way out of a nested set of loops.  This is a case where `goto` is better than the alternatives. `EXIT` is equivalent to
`break` of several other languages.

`CYCLE` skips the rest of loop and goes to the next iteration.  It is equivalent to `continue` of other languages.  Like `EXIT`, it applies only to the loop level in which it is located.

```fortran
x=1.
do while (x>0.0)
x=x+1.
if (x>=10000.0) exit
if (x<100.0) cycle
x=x+20.0
enddo
```

## Repeat-Until

The standard `DO` and `WHILE` loops test at the _top_.  That is, upon entry to the loop, the termination condition is evaluated.  If it is false, the statements of the loop are executed.  Otherwise the loop is exited.
With the ability to break out of the loop at will, we can change this pattern.
One particularly common pattern is called `repeat until`.

``fortran
do
   statement
   statement
   if (<logical expression>) exit
end do
```

One major reason for repeat-until is that a standard `while` loop may not be entered if the condition is false, whereas a repeat-until will always be executed once.

**Example**
Reading a file of unknown length.  This is _not_ how we usually read a file, since most of the time the length is known, but it is possible to construct a loop to read a file whose length may vary for different runs.
```fortran
    nlines=0
    do
        read(unit=iunit, end=10) var
        nlines=nlines+1
    enddo
10  continue
```

**Exercises**

1 Loop from 0 to 20 by increments of 2.  Make sure that 20 is included.  Print the loop variable at each iteration.
2 Start a variable n at 1.  As long as n is less than 121, do the following:
    * If n is even, add 3
    * If n is odd, add 5
    * Print n for each iteration.  Why do you get the last value?
3 Set a real value x=0. Loop from 1 to N inclusive by 1.
  * If the loop variable is less than M, add 11. to x.
  * If x > w and x < z, skip the iteration.
  * If x > 100., exit the loop.
  * Experiment with different values for the variables.  Start with N=50, M=25, w=9., z=13.

