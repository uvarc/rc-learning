---
title: "Loops and Conditionals"
toc: true
type: docs
weight: 40

menu:
    fortran_introduction:
        name: Loops and Conditionals
        weight: 40

---

# Conditionals

* elseif/else ifandelseare optional. The parentheses around the conditional are required.
* if ( comparison ) then
  * code
  * elseif( comparison) then
  * more code
  * else
  * yet more code
  * endif

# SELECT CASE

* Many else ifs can become confusing.
  * Expression must be character, integer, or logical
  * Ranges only applicable for numeric or character expressions
* SELECT CASE (expression)
* CASE(:value0)   ! Expression <= value0
* code
* CASE(value1)
  * code
  * CASE(value2)
  * code
  * CASE(value3:)  ! Expression >=value3
  * code
  * CASE (value4,value5,value6) !Multiples OK
  * code
  * CASE (value7:value9)
  * code
  * CASE DEFAULT    ! Optional
  * code
  * END SELECT

# SELECT Example

* select case (x)
  * case  (:0)
    * y=-x
  * case (1)
    * y=x+3.
  * case (2:9)
    * y=float(x)/2.
  * case (10:20)
    * y=float(x)/3.
  * case default
    * y=0.
* end select

# DO Loop

DO is the equivalent of FOR in languages like C/C++.

DO executes a fixed number of iterations unless explicitly terminated.

DO can iterate only over integer sequences.

INTEGER   :: L, U, S

INTEGER   :: I

DO I=L,U,S

…

END DO

I: Loop variable

L: Lower bound

U: Upper bound

S: Stride.  Equal to 1 if not present

Scan be negative, in which caseLmust be greater thanU.

# QUIZ

The standard requires that loop variables be integers.  How would I implement loop variables that are real?

How might real loop variables be a problem?

# Implied DO

The implied do is used in a few circumstances, specifically input/output and array construction.

(var(iterator),iterator=lbound,ubound,s)

The parentheses are required.

(a(i),i=1,20)

Implied do loops can be nested.

((r(i,j),j=1,M),i=1,N)

# Early Exit

  * exit: leave loop
  * exitis able to break out of _only_ the loop level _in which it appears_ .  It cannot break from an inner loop all the way out of a nested set of loops.  This is a case wheregotois better than the alternatives. Equivalent to Python/C/C++break
  * cycle: skip rest of loop and go to next iteration.  Equivalent to Python/C/C++continue

# WHILE Loops

* do while (<logical expression>)
  * statement
  * statement
  * …
  * end do
  * Remember that your logical expression must become false at some point.

# Example

integer  :: x, y, z

x=-20

y=-10

do while (x<0 .and. y<0)

x=10-y

y=y+1

z=0

enddo

z=1

# Exit/Cycle

x=1.

do while (x>0.0)

x=x+1.

if (x>=10000.0) exit

if (x<100.0) cycle

x=x+20.0

enddo

# Repeat-Until

do

statement

statement

….

if (<logical expression>) exit

end do

do whilealways tests at the _top_ of the loop.  Thedo…if/exitform can test anywhere.

# Example

* Reading a file of unknown length
  * nlines=0
  * do
  * read(unit=iunit, end=10)var
    * nlines=nlines+1
  * enddo
* 10 continue

* program second
* implicit none
  * integer  :: x, y, z
  * x=-20
  * y=-10
  * do while (x<0 .and. y<0)
  * x=10-y
  * y=y+1
  * z=0
  * enddo
  * z=1
  * print *, x, y, z
* end program

# Exercise

* Loop from 0 to 20 by increments of 2.  Make sure that 20 is included.  Print the loop variable at each iteration.
* Start a variable n at 1.  As long as n is less than 121, do the following:
    * If n is even, add 3
    * If n is odd, add 5
    * Print n for each iteration.  Why do you get the last value?
* Set a real value x=0. Loop from 1 to N inclusive by 1.
  * If the loop variable is less than M, add 11. to x.
  * If x > w and x < z, skip the iteration.
  * If x > 100., exit the loop.
  * Experiment with different values for the variables.  Start with N=50, M=25, w=9., z=13.

