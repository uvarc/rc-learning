---
title: "Variables"
toc: true
type: docs
weight: 25

menu:
    fortran_introduction:
        name: Variables and Types
        weight: 20

---

# Variables in Fortran

In programming, a _variable_ is similar, but not identical to, the variable familiar from mathematics.  In mathematics, a variable represents an unknown or abstract entity.  In programming, a variable represents a _location in memory_.

Computer memory consists of individual elements called _bits_, for _bi_nary dig_it_.  Each bit is "off" or "on", represented by 0 and 1.  Bits are usually grouped into units of 8, called a _byte_.  The bytes are organized into _words_.  The number of bits in a word determines whether the computer is "32 bits" or "64 bits".  Nearly all modern hardware is 64 bits, meaning that each word of memory consists of 8 bytes.  Words are numbered, starting from 0.

Each variable has a _type_.  Types are a way of representing values as patterns of bits.  Some of these types, particularly those that represent numeric values, are defined by hardware operations in the computer's CPU.  Others can be defined by the programmer, but even these derived types are represented as combinations of the _primitive types_. 
_Remember that computers do not use base 10 internally_.  

# Numeric Types: Integer

Integers are quantities with no fractional part.
They are represented by a _sign bit_ followed by the value in _binary_ (base 2).
Fortran does not support the unsigned integers of some other languages.

The default integer type has a size of 32 bits.
  * The maximum integer is $2^{32-1}=2147483648$.

Nearly all compilers offer an extension to support 64-bit integers. 

# Numeric Types: Floating Point

Floating-point numbers are representations of the mathematical real numbers.
However, due to the inherent finiteness of the computer, they have distinct 
properties.

* There is a finite number of floating-point numbers. Therefore, many (indeed, infinite) real numbers will map to the same floating-point number.
* They are represented with a form of scientific notation, so their distribution on the number line is not uniform.
* They are commutative but not associative or distributive, in general.  That 
is,
  * $r+s=s+r$
  * $(r+s)+t \ne r+(s+t)$
  * $r(s+t) \ne rs+rt$ 

Floating-point numbers are defined by the IEEE 754 standard.  They consist of a sign bit, an _exponent_, and a _significand_.  All modern hardware uses base 2 so the exponent is a power of 2.  

For input and output, these binary numbers must be converted to and from decimal (base 10), which usually causes a loss of precision at each
conversion.  Moreover, some numbers can be represented exactly given the available bits in base 10 but not in base 2 and vice versa, which is another source of error.  Finally, most real numbers cannot be represented exactly in the relatively small number of bits in either base 2 or base 10.  

The most common types of floating-point number supported by hardware are _single precision_, which occupies 32 bits, and _double precision_, which takes up 64 bits.  

|   Precision  |  Exponent Bits |  Significand Bits | Exponent Range (base 2) | Approximate Decimal Range  |  Approximate Decimal Precision |
|--------------|----------------|-------------------|-------------------------|----------------------------|
| Single       |  8    |  23  |  -126/127 | $+-2 x 10^{-38}$ to $+-3 x 10^{38} | 7 digits |
| Double       |  11   |  52  |  -1022/1023 |  $+-2.23 x 10^{−308}$ to $+-1.80 x 10^{308}$ |  16 digits |

Quad precision (128 bits) is also defined, but rarely supported in hardware by modern computers.  Most compilers support it through software, but this will be slower than hardware operations.

The IEEE 754 standard also defines several special values.  The ones most frequently encountered by programmers are `Inf` (infinity), which may be positive or negative and usually results from an attempt to divide by zero, and NaN (not a number), which is the defined result of mathematically illegal operations such as $\sqrt{-1}$.

The number of bits is _not_ a function of the OS type.  It is specified by the standard.

# Numeric Types: Complex

Fortran supports at least one complex type.  A complex number consists of 2 floating-point numbers enclosed in parentheses.

The
  * Single-precision type isCOMPLEX
  * z=(r,i)
  * Most compilers provide the
  * DOUBLE COMPLEX
  * extension as a variable type

# Non-numeric Types: Logical

Booleans are called `logical` in Fortran.
Values can be.true.or.false.(periods required)
Are not necessarily represented by integers; internal representation is up to the compiler.
Cannot even be cast to an integer.

# Non-numeric Types: Character

* Character
  * 1 byte (8 bits) per single character
* A character has a fixed length that must be declared at compile time
* character(len=8) ::mychar
* In subprograms a character of unspecified length may be passed
* character(len=*) :: dummy
  * Fortran 2008 has a variable character length but this is beyond our scope.
* Note that character in Fortran really means a fixed-length string.  The default length is 1, however.
* character :: letter

# Literals

* Literals aka constants
  * Specified values	e.g.
  * 3
  * 3.2
  * 3.213e0(Fortran single precision)
  * 3.213d0(Fortran double precision)
  * 3.213_rk(Determined by kind parameterrk)
  * "This is a string"
  * "Isn’t it true?"
  * 'Isn''t it true?'
  * .true.
  * (1.2,3.5)(Fortran complex)
* Literals have a type but it is determined from the format rather than a declaration.

In Fortran the default _literal_ is single precision.  Double precision literals _must_ include ad/Dexponent indicator.
  * Forgetting to write DP literals withDrather thanEoften causes a significant loss of precision that is hard to find.

