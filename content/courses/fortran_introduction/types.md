---
title: "Variable Types"
toc: true
type: docs
weight: 25

menu:
    fortran_introduction:
        name: Variable Types
        weight: 25

---

# Variables in fortran

Unlike most languages, Fortran is _not_ case sensitive.  VariablesMean,mean, and evenmEanare the same to the compiler.

Like most compiled languages, Fortran is _statically_  _typed_ .  All variables must be _declared_ to be of a specific type before they can be used.  A variable’s type cannot be changed once it is declared.

Fortran is (nearly) strongly typed.  Mixed-mode expressions are limited and most conversions must be explicit.

# Numeric Types: Integer

* Integer
  * Quantities with no fractional part
  * Represented by sign bit + value in _binary_
    * _Remember that computers do not use base 10 internally_
    * Default integers are of size 32 bits
  * Maximum integer is is 232-1
    * All Fortran integers are signed
  * Compiler extension (in nearly all compilers)
    * INTEGER*8  (old declaration style) is a 64-bit integer
    * Will show another method when we learn about KIND

# Numeric Types: Single Precision

* Floating point single precision
  * CalledREALin Fortran
  * Sign, exponent, mantissa
  * 32 bits
  * IEEE 754 defines representation and operations
  * Approximately 6-7 decimal digits of precision, _approximate_ exponent range is 10-126to 10127

# Numeric Types: Double Precision

* Double precision floating point
  * Sign, exponent, mantissa
  * 64 bits
    * Number of bits NOT a function of the OS type!  It is specified by the IEEE 754 standard!
  * Approximately 15-17 decimal digits of precision, approximate exponential range 10-308to 10308
  * In Fortran the default _literal_ is single precision.  Double precision literals _must_ include ad/Dexponent indicator.
  * Forgetting to write DP literals withDrather thanEoften causes a significant loss of precision that is hard to find.

# Numeric Types: Complex

* A complex number consists of 2 reals enclosed in parentheses
  * Single-precision type isCOMPLEX
  * z=(r,i)
  * Most compilers provide the
  * DOUBLE COMPLEX
  * extension as a variable type

# Non-numeric Types: Logical

* Booleans are calledlogicalin Fortran.
* Values can be.true.or.false.(periods required)
  * Are not necessarily represented by integers; internal representation is up to the compiler.
  * Cannot even be cast to an integer.

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

