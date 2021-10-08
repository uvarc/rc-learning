---
title: "Variables and Types"
toc: true
type: book
weight: 21

---

In programming, a _variable_ is similar, but not identical to, the variable familiar from mathematics.  In mathematics, a variable represents an unknown or abstract entity.  In programming, a variable represents a _location in memory_.

Computer memory consists of individual elements called _bits_, for 
*bi*nary dig*it*.  Each bit is "off" or "on", represented by 0 and 1.  Bits are usually grouped into units of 8, called a _byte_.  The bytes are organized into _words_.  The number of bits in a word determines whether the computer is "32 bits" or "64 bits".  Nearly all modern hardware is 64 bits, meaning that each word of memory consists of 8 bytes.  Words are numbered, starting from 0.

Each variable has a _type_.  Types are a way of representing values as patterns of bits.  Some of these types, particularly those that represent numeric values, are defined by hardware operations in the computer's CPU.  Others can be defined by the programmer, but even these derived types are represented as combinations of the _primitive types_.
_Remember that computers do not use base 10 internally_.

_Precision_ is the number of digits that are accurate, according to the requirements of the IEEE standard.  Please note that compilers will happily output more digits than are accurate if asked to print unformatted values.

Like most programming languages, C++ is _case sensitive_.  Variables `Mean` and `mean` and even `mEan` are different to the compiler.  

Moreover, like most compiled languages. C++ is _statically typed_ .  All variables must be _declared_ to be of a specific type before they can be used.  A variable’s type cannot be changed once it is declared.

C++ is _nearly_ strongly typed.  Mixed-mode expressions are limited and most conversions must be explicit.

### Naming

Variable names must consist of only letters from the Latin alphabet, digits, or underscores.  They must begin with a letter or an underscore.  Spaces and special characters other than the underscore are not permitted.  The number of characters in a name is limited only by the system (compiler and platform) but programmers are advised to choose names that are descriptive but not overlong.

Good descriptive names may consist of several words or parts of words.  Since C++ is case-sensitive, a popular way to distinguish the segments is _camel case_.
This omits underscores and uses capitalization to separate components.  The customary version for most C++ programmers starts the name lower case and capitalizes subsequent components, with the possible exception of the names of classes.
```c++
isValid
startDate
class Animal
```
Variables and other _identifiers_ may not be the same as the list of _reserved words_ in C++. A list defined by the standard is [here](https://en.cppreference.com/w/cpp/keyword).  Some compilers may define additional keywords.

## Basic Types

### Integers

Integers are quantities with no fractional part.  C++ supports _signed_ and _unsigned_ integers.  Signed integers take on all values within the available range.  Unsigned integers represent only nonnegative values.

Signed integers are represented internally by a sign bit followed by a value in _binary_.  Remember that computers _do not_ use base 10 internally.
Unsigned integers omit the sign bit and use all the available bits for the value.

C++ supports several categories of integer, differing by the number of bits to represent them and whether they are signed or unsigned.
The C++ standard does not specify the number of bits in an integer, only the _minimum_ for each category.

|    Declaration    | Minimum Number of Bits   |  Minimum Range  |
|-------------------|--------------------------|-----------------|
|    short          |      16              |  -32,768 to 32,767   |
|  unsigned  short  |      16              |  0 to 65,535         |
|  int              |      16, usually 32  |  -32,768 to 32,767   |
|  unsigned int     |      16, usually 32  |  0 to 65,535         |
|  long             |      32              |  -2,147,483,648 to 2,147,483,647  |
|  unsigned long    |      32              |  0 to 4,294,967,295               |
|  long long        |      64              | -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807  |
|  unsigned long long |    64              |  0 to 18,446,744,073,709,551,615  |

The programmer can use the `sizeof` function to determine the actual size of a type on the system in use.  It returns a result in bytes.  On most computing platforms, the `int` and `long` are the same (32 bits or 4 bytes).

{{< code file="/courses/cpp_introduction/codes/ints.cxx" lang="c++" >}}

The output for this code on an Intel-based Linux computer using the g++ compiler was
```no-highlight
2,4,8,2,8,8,8
```

### Floating Point Numbers

loating-point numbers are representations of the mathematical real numbers.
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
As for integers, the C++ standard specifies only _minimum_ ranges.  On nearly all general-purpose hardware, single-precision floating point is `float` and double-precision floating point is `double`.

|   Precision  |  Exponent Bits |  Significand Bits | Exponent Range (base 2) | Approximate Decimal Range  |  Approximate Decimal Precision |
|--------------|----------------|-------------------|-------------------------|----------------------------|--------------------------------|
| Single       |  8    |  23  |  -126/127 | &plusmn;2 x 10<sup>-38</sup> to &plusmn;3 x 10<sup>38</sup> | 7 digits |
| Double       |  11   |  52  |  -1022/1023 |  &plusmn;2.23 x 10<sup>−308</sup> to &plusmn;1.80 x 10<sup>308</sup> |  16 digits |

C++ specifies a `long double` type but requires only that it be at least equivalent to double.

{{< code file="/courses/cpp_introduction/codes/floats.cxx" lang="c++" >}}

The output of the above program on the same Intel-based Linux computer with g++ was
```no-highlight
4,8,16
```

The IEEE 754 standard also defines several special values.  The ones most frequently encountered by programmers are `Inf` (infinity), which may be positive or negative and usually results from an attempt to divide by zero, and NaN (not a number), which is the defined result of mathematically illegal operations such as $\sqrt{-1}$.

The number of bits is _not_ a function of the OS type.  It is specified by the standard.

## Boolean

Booleans represent truth value.  The name of the type is `bool` and the only permitted values are `true` and `false`.

Internally, true is 1 and false is 0, but it’s easier for humans to read and remember true/false.

```c++
bool isValid;
```
Since they are integers they can be used in mathematical expressions, though this can become confusing.  

## Literals

_Literals_ are specific values corresponding to a particular type.  The compiler infers the type from the format.

|  Value  |  Type |
|---------|-------|
|  3      |  int  |
| 3.2     | double |
| 3.213e0 | double |
| "This is a string" | string |
| "Isn’t it true?" | string |
| true    |  bool  |

Note that the default type for a floating-point literal is a `double`.
