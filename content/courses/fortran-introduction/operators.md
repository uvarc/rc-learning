---
title: "Operators, Expressions, and Type Conversions"
date : "2021-04-5T00:00:00-05:00"
toc: true
type: book
weight: 24

menu:
    fortran-introduction:
        parent: Operators, Expressions, and Type Conversions
        weight: 24

---

Operators are characters or groupings of characters that take some number of variables or literals as _operands_, apply some specific mathematical, logical, or translational operation, and return a result.  Operators are defined by each programming language, although basic ones are often the same or similar.  The majority are mathematically binary operators, i.e. they take two operands, though nearly all languages have some unitary operators and a few have operators that take three or more operands.  Each operand must be of the specific types for which an operator is valid.

## Basic Operators

### Arithmetic Operators

These operators are defined on integers, floats, and doubles.  

`+ -` add, subtract

`* /` multiply, divide

`**` exponentiation

Operators are applied in a particular order.  This is called precedence.
First: ** 
Second (equal status):  * /
Third (equal status):  + -

Evaluation is left to right by precedence unless parentheses are used to 
specify a different ordering.
```
5+11*6=71
(5+11)*6=96
```
The mnemonic *PEMDAS* is sometimes applied--*P*arentheses*E*xponents*M*ultiplication*D*ivision*A*ddition*S*ubtraction--but remember that MD and AS are equals within their ranking.

Not all programming languages have an exponent operator.  The base and exponent may both be integers or floating-point numbers.
**Handy Trick**
Many Fortran compilers will recognize integer exponents and, at least for relatively small ones, will perform a multiplication, whereas floating-point exponents are evaluated with the much slower logarithm functions.  Always remember that a literal like `3.0` is a _floating point_ number, **not** an integer.

Right: `x**3`
Wrong: `x**3.0`

### Special Integer Operators

Division.  In Fortran `2/3` is always zero!  Why?
This is because 2 and 3 are both integers, so `/` is an integer operation that yields an integer result.  This is a frequent source of bugs in compiled languages.

Exercise:
What is 9/5?

Remainders.
MOD(N,M).  The `mod` intrinsic function returns the remainder of a division.  It is computed as `N-(INT(N/M)*M)`.

MODULO(N,M). The `modulo` intrinsic function returns N mod M, which is computed as `N-FLOOR(N/M)*M`. 

Points to note:
  * MOD and MODULO are _not the same_ for negative numbers.
  * MOD is most frequently used though MODULO is closer to other languages' `%` operator.  
  * Use for negatives is uncommon in all languages.

`Mod` and `modulo` are defined for negative values and reals, as well as nonnegative integers, but the results, while well-defined mathematically, are not generally what most programmers are expecting.  For this reason they should generally be avoided for arguments other than nonnegative integers.

**Example**
{{< code-download file="/courses/fortran-introduction/codes/testmod.f90" lang="fortran" >}}

## Expressions

An _expression_ is a combination of variables, operators, and function invocations that can result in a unique evaluation.

Fortran expressions are much like those of other languages.
```fortran
a+3*c
8.d0*real(i,dp)+v**3
phase+cmplx(0.,1.)
sqrt(abs(a-b))
A .or. B
y > 0.0 .and. y < 1.0
myfunc(x,y)
```

## Type Conversions

As we have seen with the example of dividing two integer, operators are defined on specific types and return a specific type.  What if we write `2./3`?  The first operand is a real, whereas the second is an integer.  This is called a _mixed expression_.  For consistency, one type must be converted to match the other before the operator is applied.  Type conversion is also called _casting_.

Most compilers will automatically cast numeric variables in mixed expressions.  The variables are _promoted_ according to their rank.  Lowest to highest rank, the types are integer, real, double, complex.  Therefore, integers will be converted to float if necessary, floats to double precision, then to complex.

The rules for numerical type conversions may result in some surprises.  For example, when a real is converted to double precision, the extra bits in the significand are filled ("padded") with zeros.  There is no magic that tells the compiler how to extend it "correctly."  To illustrate with a base-10, 5-digit example:
```fortran
real             :: r
double precision :: s
r=1./3.
s=r
```
We would find that, in this (artificial) number system,
```fortran
r=.33333
1.d0/3.d0=.3333333333
s=.3333300000
```

Fortran, like most programming languages, also provides means for the programmer to specify when a type conversion should take place.
Use this explicit casting to be clear, or in circumstances, such as argument lists, where the compiler will not do it.

The new way to cast numbers is via [KIND](/courses/fortran-introduction/primitive_types).  Older conversion functions such as `dble` can still be used and will usually be present in older code.

Logicals cannot be cast to anything, even though they are usually represented internally by integers.

**Examples**
Explicit casting among numeric types, default kind.
```
R=REAL(I)
I=INT(R)
Z=CMPLX(r1,r2)
D=DBLE(R)
```
Using KIND (with predetermined parameters)
```
R=REAL(I,dp)
D=REAL(R,dp)
```

### Character/Numeric

Fortran has a peculiar way to do this called _internal read/write_.

Convert numeric to character:
```
character(len=4)  :: age
integer           ::iage
  iage=39
  write(age,'(i4)') iage
```
Convert character to numeric:
```
   age='51'
   read(age,'(i4)') iage
```

The character variable to be converted always appears as the first argument to the read or write.  It is called a _buffer_.
To remember whether to use read or write, keep in mind that if we wish to convert numeric to character we know the number but not the character, so we will _write_ it to the buffer.  For character to number, we will _read_ the known characters from the buffer and write them into the target numeric variable.
