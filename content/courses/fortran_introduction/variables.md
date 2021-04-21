---
title: "Variables and Operators"
toc: true
type: docs
weight: 20

menu:
    fortran_introduction:
        name: Variables and Operators
        weight: 20

---

In programming, a _variable_ is similar, but not identical to, the variable familiar from mathematics.  In mathematics, a variable represents an unknown or abstract entity.  In programming, a variable represents a _location in memory_.  

Computer memory consists of individual elements called _bits_, for _bi_nary dig_it_.  Each bit is "off" or "on", represented by 0 and 1.  Bits are usually grouped into units of 8, called a _byte_.  The bytes are organized into _words_.  The number of bits in a word determines whether the computer is "32 bits" or "64 bits".  Nearly all modern hardware is 64 bits, meaning that each word of memory consists of 8 bytes.  Words are numbered, starting from 0.  

# Variable Declarations

* Variables are declared by indicating the type followed by a comma-separated list of variables.
* In older code no separator was used.
  * INTEGERi, j, k
* In newer code (including all new code you write) use the double colon to separate the type from the variable list
* INTEGER  ::i, j, k
* If there are other attributes on the line the::will be _required_ .

# Declarations

First statement should be

PROGRAMmyname

Then follow it immediately with

IMPLICIT NONE

Declare variables with double-colon syntax

INTEGER              ::   I, J

REAL                 ::   R, S, T

DOUBLE PRECISION     ::   D

DOUBLE COMPLEX       ::   Z

LOGICAL              ::   FLAG

CHARACTER (len=20)   ::   C

Line up declarations neatly.

All caps are __not__ required but I use them to emphasize the keywords.

# Initializing at Compile Time

Variables can be declared and initialized at the same time:

real  :: x=1.e-8, y=42.

When variables are initialized in this manner it happens only _once_ , at compile time.  If this takes place in a subprogram it will not happen again upon repeated invocations.

It is equivalent to the olderDATAstatement

datax,y/1.e-8,42./

In Fortran 2003 it became possible to initialize using intrinsic functions:

real  :: pi = 4.0*atan(1.0)

# Example

* StartGeany, Code::Blocks, or another editor.  Type
* program first
* ! My first program
* ! Author:  Your Name
* implicit none
  * real     ::x,y
  * integer  ::i,j=11
  * x=1.0
  * y=2.0
  * i=j+2
  * print *, "Reals are ",x,y
  * print *, "Integers are ",i,j
* end program

# PARAMETER

In compiled languages, programmers can declare a variable to have a fixed value that cannot be changed.

In Fortran this is indicated by thePARAMETERattribute.

real, parameter  ::  pi=3.14159

Attempting to change the value of a variable declared to be a parameter will result in a fatal compiler error.

In older code the declaration and parameter statement will be on different lines

real  pi

parameter (pi=3.14159)

# Type Conversions

Most compilers will automatically cast numeric variables to make mixed expressions consistent.  The variables are promoted according to their rank.  Lowest to highest the types are integer, float, double, complex.

Use explicit casting to be clear, or in circumstances such as argument lists where the compiler will not do it.

The new way to cast numbers is viaKIND.  Older conversion functions such asdblecan still be used and will be in older code.

Logicalscannot be cast to anything.

Strings may be cast to numbers and vice versa by a fairly idiosyncratic method.

# Examples

Explicit casting among numeric types, default kind.

R=real(I)

I=int(R)

Z=cmplx(r1,r2)

D=dble(R)

# Character  Numeric

* Fortran has a peculiar way to do this called internal read/write.
* Convert numeric to character:
* character(len=4)  :: age
* integer           ::iage
  * iage=39
  * write(age,'(i4)')iage
* Convert character to numeric
* age='51'
* read (age,'(i4)')iage

# Arithmetic Operations

Operators are defined on integers, floats, and doubles

+ -add subtract

* /multiply divide

**exponentiation

Operator Precedence is:

**  (* /) (+ -)

Evaluation is left to right by precedence unless told otherwise with parentheses

# Integer Operators

* In Fortran 2/3 is always zero!  Why?
  * Because 2 and 3 are both integers.  Nothing will be promoted to a float, so / is an integer operation that yields an integer result
* Remainder comes frommod(n,d)ormodulo(n,d)
  * modandmoduloare NOT THE SAME for negative numbers
  * modis most frequently used thoughmodulois closer to other languages'%operator.  Use for negatives is uncommon in all languages.

# Character (String) Operators

* Strings/Characters
  * There are many (some of which require function calls)
  * Concatenation//
  * Substring extraction
  * S(1:3)
  * The first character is counted as 1 and the last one in the substring is the actual upper bound.  This expression extracts characters 1 to 3 _inclusive_ .
  * Fortran counts from 1 and the upper bound is includedin the range.

# Conditional Operators

* Numeric
  * Fortran has two sets, one with letters and one with symbols.  Note that/=has a/for “not.”
    * .eq.  ==
    * .ne.  /=
    * .lt.  <
    * .gt.>
    * .le.  <=
    * .ge.  >=

# Logical Operators

* Negation
  * .not.
    * .not. flag
* AND
  * .and.
* OR
  * .or.

# Conditional Operator Precedence

>,>=,<,<=outrank==or/=

==,/=outranks .and.

.and.outranks .or.

As always, use parentheses to change grouping or to improve clarity.

# Character Comparison Intrinsics

lge(stringA,stringB)

Returns.true.IfstringAis lexically greater than or equal tostringB, otherwise returns.false.

lgt(stringA,stringB)

Returns.true.IfstringAis lexically greater thanstringB, otherwise returns.false.

lle(stringA,stringB)

Returns.true.IfstringAis lexically less than or equal tostringB, otherwise returns.false.

llt(stringA,stringB)

Returns.true.IfstringAis lexically less than or equal tostringB, otherwise returns.false.
