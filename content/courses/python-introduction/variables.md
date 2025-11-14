---
title: Variables and Types
toc: true
type: docs
draft: false
weight: 20
date: "2020-11-17T00:00:00"
menu:
    python-introduction:
        parent: The Basics
---

Variables in a computer program are not quite like mathematical variables.  They are placeholders for _locations in memory_.  Memory values consists of a sequence of binary digits (bits) that can be `0` or `1`, so all numbers are represented internally in __base 2__.  Eight bits is a _byte_, another frequently used unit in computing.  Memory is organized into chunks called _words_; most modern computers use 64-bit (8 byte) words.   

Names of variables are chosen by the programmer.  Python is case-sensitive, so `myVariable` is not the same as `Myvariable` which in turn is not the same as `MyVariable`.  With some exceptions, however, the programmer should avoid assigning names that differ only by case since human readers can overlook such differences.

Variable names must use only letters of the Latin alphabet, digits, or underscores.  A variable name must begin with a letter or underscore.  Special symbols other than the underscore are not allowed.  By convention, variable names beginning with underscores are regarded as "special" and are reserved for certain circumstances, such as internal variables in [classes](/courses/python-introduction/classes).  Names surrounded by double underscores (sometimes called "dunders") generally are used by internal Python operations. 

## Python Types

Variables always have a __type__ even if it is not explicitly declared.  The __primitive types__ correspond more or less to the types handled directly by the hardware, specifically integers, floating-point numbers, and characters.  Many languages define a number of other simple types including Booleans, strings, complex numbers, and so forth.  Python defines several primitive types and has some built-in _compound types_.

### Integers

Integers are whole numbers and are not written with a decimal point.  In Python an integer can be of any size but numbers larger than what can be represented in the hardware will be handled by software and can be slow. On modern systems the hardware size is 64 bits.  Only signed integers are supported (so any integer may be positive or negative).

### Floating-Point Numbers

Floating-point numbers have a decimal point, i.e. a fractional part.  A standard called IEEE 754 defines the way these numbers are represented as base-2 numbers with a specific, and finite, number of bits for each value.  They are represented internally by a variant of scientific notation, to base 2, with one bit for the sign and the rest for the _significand_ and the _exponent_. In most languages there are two types of floating-point numbers, single precision and double precision.  Single precision numbers are 32 bits long in total.  Double precision numbers occupy 64 bits.  Most of the time a Python floating-point variable is double precision.  Only in a few packages, mainly NumPy, is a single-precision floating-point number available.

A double-precision floating-point number has an exponent range, in base 10, of approximately 10<sup>-308</sup> to 10<sup>308</sup> and a decimal precision of about 15-16 digits.  The first thing to note is that this is _finite_.  The number of mathematical real numbers is infinite, and they must all be represented by the finite number of floating-point values.  It should be obvious, then, that infinitely many real numbers will map to the same floating-point number.  It also follows that only integers or terminating rational numbers can be exactly represented at all.  All other numbers are approximated.  Some numbers that are terminating in base 10 are not terminating in base 2 and vice versa.  Floating-point numbers do not obey all the rules of mathematical real numbers; in particular, they are commutative (so $a+b = b+a$) but are not necessarily associative (i.e. $a + (b+c)$ may not equal $(a + b) + c$) and they are generally not distributive (so $a \times (b-c)$ is not necessarily equal to $a \times b-a \times c$).  For most work these properties of floating-point numbers do not matter, but for some types of scientific programs this behavior can be important.

The floating-point standard also defines special values INF (and -INF) and NAN.  INF or -INF means the absolute value of the number is too large to be represented.  NAN stands for "Not a Number" and is returned when a program attempts to perform a mathematically illegal operation, such as dividing by zero.  It is also frequently used in some Python packages to represent missing data.

### Complex

Python supports complex numbers.  A complex number consists of two double-precision real numbers and is expressed in the form

```python
R+I*1J
```
or
```python
R+I*1j
```
The imaginary part is denoted by the letter "J" (not "i" as in most mathematics) and it is not case-sensitive in this context.  The numerical value of the imaginary part must immediately precede it with no multiplication symbol.  If the imaginary part is a variable, as in the examples, the digit 1 must be present.  This is so the interpreter knows that the J indicates the square root of -1, and is not a variable.

### Boolean

Boolean variables indicate _truth value_.  Booleans have only two possible values, `True` or `False` (note the capitalization).  Internally Booleans are integers, but this is (usually) not important to the programmer.  Use Booleans when a variable naturally represents a value that can be expressed as true/false or yes/no.

## Choosing Good Variable Names

Variable names can improve or reduce the readability of a script or code.  Interpreters do not have problems keeping track of variables or following the logical flow of a code, but human readers do often have these problems.  Thinking of the script as text will help guide choices for variable and other names.  A variable name should be descriptive of what the variable represents without being too long. It is acceptable and common to include two words, but they should be separated either capitalizing some or all letters beginning a word or by underscores.  Some choices may be better for different types; for instance, the name of a Boolean may be a phrase that can be answered "yes" or "no."

```
isValid=True
is_valid=False
```
The first form in the example above is called _camel case_, which uses capitalization to separate parts of a long variable name. It gets its name from a fanciful comparison of the ups and downs of the case changes to a multi-humped camel.  The other convention uses underscores for purposes of clarity.  Camel case and underscores may be used together; choose whichever seems clearer given the context, though there tends to be a preference in Python for underscores for ordinary variables.  However, it is conventional to use camel case for certain advanced constructs such as _classes_.

## Literals

Literals are specific values, as distinct from variables.  Literals also have a type, which is determined by their format.  Examples:

* `3` (integer)
* `3.2` (floating point)
* `1.234e-25` (floating point, exponential notation)
* `"This is a string"` (Python string)
* `True` (Python Boolean)
* `1.0+2J` (Python complex)

## Type Conversions

Type conversions, also known as _casts_, are used when it is necessary to change a variable's type from one to another.  Python assigns types to variables by _inference_; that is, it determines from context what the type should be.  If the programmer needs it to be something different, an explicit cast can be used.

```python
n=int(f)
f=float(n)
```

Often an arithmetic expression contains variables of more than one type. If no explicit casts are specified, the interpreter promotes each variable to the highest-rank type in the expression.  The hierarchy from top to bottom is complex, float (double), then integer.

In Python it is easy to convert a string to the number it represents, and vice versa.  No special "internal buffers" are required.  It is, however, important that the string represent a valid number, and it must match the type to which it is being cast.

```python
x=float("11.3")
ind=int("11")
y=int("11.") #wrong, an error will result

amount=str(x+.4)
```
