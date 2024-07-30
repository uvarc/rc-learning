---
date: "2021-04-05"
title: "Variables and Types"
weight: 21
---

In programming, a _variable_ is similar, but not identical to, the variable familiar from mathematics.  In mathematics, a variable represents an unknown or abstract entity.  In programming, a variable represents a _location in memory_.

Computer memory consists of individual elements called _bits_, for _bi_nary dig_it_.  Each bit is "off" or "on", represented by 0 and 1.  Bits are usually grouped into units of 8, called a _byte_.  The bytes are organized into _words_.  The number of bits in a word determines whether the computer is "32 bits" or "64 bits".  Nearly all modern hardware is 64 bits, meaning that each word of memory consists of 8 bytes.  Words are numbered, starting from 0.

Each variable has a _type_.  Types are a way of representing values as patterns of bits.  Some of these types, particularly those that represent numeric values, are defined by hardware operations in the computer's CPU.  Others can be defined by the programmer, but even these derived types are represented as combinations of the _primitive types_. 
_Remember that computers do not use base 10 internally_.  

_Precision_ is the number of digits that are accurate, according to the requirements of the IEEE standard.  Please note that compilers will happily output more digits than are accurate if asked to print unformatted values.

## Numeric Types

### Integer

Integers are quantities with no fractional part.
They are represented by a _sign bit_ followed by the value in _binary_ (base 2).
Fortran does not support the unsigned integers of some other languages.

The default integer type has a size of 32 bits.
The range of this type is -2,147,483,648 to 2,147,483,647.  

Nearly all compilers offer an extension to support 64-bit integers. 
Their range is -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807.
Integers are represented exactly as long as they fit within the range.

### Floating Point

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

| Precision | Exponent Bits | Significand Bits | Exponent Range (base 2) | Approximate Decimal Range                                           | Approximate Decimal Precision |
|-----------|---------------|------------------|-------------------------|---------------------------------------------------------------------|-------------------------------|
| Single    | 8             | 23               | -126/127                | &plusmn;2 x 10<sup>-38</sup> to &plusmn;3 x 10<sup>38</sup>         | 7 digits                      |
| Double    | 11            | 52               | -1022/1023              | &plusmn;2.23 x 10<sup>−308</sup> to &plusmn;1.80 x 10<sup>308</sup> | 16 digits                     |

Quad precision (128 bits) is also defined, but rarely supported in hardware by modern computers.  Most compilers support it through software, but this will be slower than hardware operations.

The IEEE 754 standard also defines several special values.  The ones most frequently encountered by programmers are `Inf` (infinity), which may be positive or negative and usually results from an attempt to divide by zero, and NaN (not a number), which is the defined result of mathematically illegal operations such as $\sqrt{-1}$.

The number of bits is _not_ a function of the OS type.  It is specified by the standard.

### Complex

Fortran supports at least one complex type.  A complex number consists of 2 floating-point numbers enclosed in parentheses.
The single-precision type is COMPLEX.  It is represented as `z=(r,i)`.
Most compilers provide the DOUBLE COMPLEX extension as a variable type.

## Non-numeric Types

### Logical

Boolean variables represent  "true" or "false."  They are called `logical` in Fortran.
Their values can be `.true.` or `.false`. The periods are required.
In some languages Booleans are actually integers; in Fortran that is not necessarily the case; the internal representation is up to the compiler.
Logicals cannot even be converted to an integer in Fortran.

### Character

Characters used in Fortran code are [ASCII](http://www.asciitable.com/). Fortran supports Unicode to a very limited extent; it is available only in comments and printing and uses the [Universal Coded Character Set](https://en.wikipedia.org/wiki/Universal_Coded_Character_Set), which does not support all features of Unicode, and not all compilers support this.
The length may be declared at compile time with a default length of 1, representing a single symbol.  The Fortran 2003 standard introduces a variable-length character (string).

## Literals

_Literals_ are specific values corresponding to a particular type.

**Examples**

| Value              | Type                                          |
|--------------------|-----------------------------------------------|
| 3                  | Integer                                       |
| 3.2                | Single precision floating point               |
| 3.213e0            | Single precision floating point               |
| 3.213d0            | Double precision floating point               |
| 3.213_rk           | Type determined by `kind` parameter `rk`      |
| "This is a string" | Character string                              |
| "Isn’t it true?"   | Character string                              |
| 'Isn''t it true?'  | Character string                              |
| .true.             | Logical                                       |
| (1.2,3.5)          | Single precision complex                      |
| (1.2d0,3.5d0)      | Double precision complex (compiler extension) |

In Fortran the default floating-point literal is _single precision_.  Double precision literals _must_ include a d/D exponent indicator.  This is different from most languages, included C/C++, for which the default floating-point literal is double precision.
Forgetting to write double-precision literals with `D` exponent indicator rather than `E` often causes a significant loss of numerical precision that is hard to find.

## KIND

Before the IEEE standard was universally adopted, some computer systems used a 64-bit floating-point number for REAL and included hardware for 128-bit DOUBLE PRECISION, while other systems used 32-bit REALs and 64-bit DOUBLE PRECISION.  This made porting codes back and forth problematic.  KIND was developed to solve this problem.  Rather than requesting REAL or DOUBLE PRECISION, the programmer could specify the minimum precision (for floating point) or range (for integer).  This motivation was mooted with the move to IEEE 754, but KIND remains useful as an abstract way to specify the desired type.  Put simply, an integer can be associated with the different precisions and ranges that are possible for primitive types.  A given compiler does not need to support all possibilities but should return an indication that it does not support a requested type.

The intrinsics SELECTED_REAL_KIND and SELECTED_INT_KIND can be used to specify KIND.  For compilers that support an additional character set, SELECTED_CHARACTER_KIND can be used to print most Unicode characters.
```
SELECTED_REAL_KIND(P,R)
```
requests a REAL with a decimal precision of at least P digits and an exponent range of at least R.  Fortran 2008 allows an additional argument RADIX to select the base for the other options.
```
SELECTED_INT_KIND(R)
```
requests an INTEGER with a range at least 10<sup>-R</sup> to 10<sup>R</sup>.
Both of these intrinsics should return negative values if the request cannot be accommodated.  For example,
```fortran
integer, parameter :: ik=selected_int_kind(20)
```
returns -1 on most systems, since it is outside the range of a 64-bit signed integer.  To specify a 64-bit integer without using an older notation, use something like
```fortran
integer, parameter :: ik=selected_int_kind(15)
```

The value returned must be declared INTEGER, PARAMETER to be used in [variable declarations](/courses/fortran-introduction/declarations).
An [intrinsic module](/courses/fortran-introduction/intrinsic_modules) can be used to obtain the KIND parameters.

The KIND intrinsic can be used to return the KIND of a particular variable or literal.  This can also be used to set KIND parameters.
```
KIND(v)
```
For example
```
INTEGER, PARAMETER :: dp=KIND(1.0d0)
REAL(dp)           :: x
```
See the [Intel documentation](https://software.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/data-types-constants-and-variables/intrinsic-data-types.html#intrinsic-data-types) for examples for their compiler.

