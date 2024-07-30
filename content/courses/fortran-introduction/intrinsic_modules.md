---
date: "2021-04-05"
title: "Intrinsic Modules"
weight: 72
---

Recent revisions of the Fortran standard support several intrinsic modules.  
One must be downloaded, while the others can be USEd and will be supplied by the compiler.

## ISO_VARYING_STRING

Fortran 95 did not support a variable-length string.  A standardized module was defined to support a type VARYING_STRING.  This module was never incorporated into the standard, so compilers do not include it, but an implementation can be found [here](http://www.astro.wisc.edu/~townsend/static.php?ref=iso-varying-string).  This module was mostly obsoleted by the variable-string capabilities of Fortran 2003, but does have a few features still lacking in the standard.  A description is [here](http://numat.net/fortran/is1539-2-99.html).  In addition to defining the standard character intrinsics for VARYING_STRING, it contains some additional functionality, such as GET and PUT to read and write a character from or into a string, REMOVE, REPLACE, SPLIT, and some other useful procedures.
This module should be USEd like a programmer-written module.

## Intrinsic Modules

The Fortran 2003 standard defined several intrinsics modules.  They have a special form of USE:
```
USE, INTRINSIC :: <module>
```

## ISO_FORTRAN_ENV

This module contains many useful variables for system parameters, some storage parameters, and KIND parameters.  It also provides two intrinsic functions, COMPILER_OPTIONS and COMPILER_VERSION, which respectively return the command-line options and the compiler version used to compile the binary.
```fortran
use iso_fortran_env

print *, "This executable was compiled with ",COMPILER_VERSION()
print *, "The options used were ",COMPILER_OPTIONS()
```
Particularly useful members of the ISO_FORTRAN_ENV module are predefined KIND parameter for specific types.  
```fortran
USE ISO_FORTRAN_ENV
INTEGER(int64)     :: i,j
```

| KIND Parameter | IEEE Type             |
|----------------|-----------------------|
| int8           | 8-bit integer         |
| int16          | 16-bit integer        |
| int32          | 32-bit integer        |
| int64          | 64-bit integer        |
| real32         | 32-bit floating point |
| real64         | 64-bit floating point |
| real128        | 128-bit real          |

Not all the IEEE KINDs may be supported in the hardware, particularly real128.
Not all compilers support all the above KINDs, in which case it should set it to a negative value.

## IEEE Modules

The three IEEE modules provide constants and procedures relating to floating-point hardware such as exceptions and arithmetic constants.  
A good reference for these modules is provided by Intel for their [compiler](https://software.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/program-units-and-procedures/intrinsic-modules/ieee-intrinsic-modules-and-procedures/ieee-intrinsic-modules-quick-reference-tables.html#ieee-intrinsic-modules-quick-reference-tables).  The NAG compiler also has useful [documentation](https://www.nag.com/nagware/np/r70_doc/manual/compiler_9_6.html#AUTOTOC_9_6).

### IEEE Features

This module specifies the definitions of IEEE special bit patterns such as IEEE_Datatype and IEEE_Inf.

### IEEE Exceptions

An _exception_ occurs due to an illegal operation.  This may include mathematically illegal operations such as taking the square root of a negative real number, dividing by zero, and so forth.  Due to the finite range of floating-point numbers, other excepts are _underflow_ and _overflow_.  Mathematically illegal operations result in NaN (Not a Number), whereas overflow results in Inf.  Operations are defined on NaNs, with the result of any arithmetic operation on a NaN being another NaN.  Therefore, NaNs can easily propagate through results and it would be useful to catch them when they first occur.  The IEEE Exceptions module can help with this.
For example, the IEEE_SET_HALTING_MODE,HALTING) intrinsic would be invoked as follows:
```fortran
USE, INTRINSIC :: IEEE_EXCEPTIONS
TYPE(IEEE_FLAG_TYPE) :: flag
LOGICAL              :: halt=.true.

CALL IEEE_SET_HALTING_MODE(flag,halt)
```
`Flag` is a variable of TYPE(IEEE_FLAG_TYPE), also defined in the module.  It can be IEEE_DIVIDE_BY_ZERO, IEEE_INEXACT, IEEE_INVALID, IEEE_OVERFLOW, or IEEE_UNDERFLOW.  The `HALTING` argument is LOGICAL.  If HALTING is set to `.true.` the program will stop on occurrence of the specified MODE. Since this is often desired for INVALID (NaN), DIVIDE_BY_ZERO, and OVERFLOW, a predefined array is available.

{{< code-download file="courses/fortran-introduction/codes/ieee_exc.f90" lang="fortran" >}}

### IEEE Arithmetic

The IEEE_ARITHMETIC encompasses and extends the IEEE_EXCEPTIONS module.
Several useful procedures and included. IEEE_SELECTED_REAL_KIND chooses only KINDs corresponding to IEEE-supported types. Other procedures determine whether arithmetic operations conform to IEEE specifications.  

For more elegant handling of errors, the IEEE_IS_NAN, IEEE_IS_FINITE, and some others can be used to test a result and handle it in some manner other than halting the execution.

{{< code-download file="courses/fortran-introduction/codes/ieee_arith.f90" lang="fortran" >}}

## ISO_C_BINDING

Mixed-language programming is common, particularly mixing C with other languages.  In the past, invoking C procedures from Fortran was tedious and error-prone, due to differences in conventions such as character termination, name-mangling of subprograms, and so forth.  The ISO_C_BINDING module was added to simplify this.

A good reference for the content of this module is from [gfortran](https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html#ISO_005fC_005fBINDING). Note that this module provides C equivalents to Fortran types for a given platform; in particular, the C standard does not specify the length of an `int`, only a minimum, so on some platforms the default is 16 bits and on others it is 32 bits.  The variables defining the correspondence can be used as KIND parameters.  A subset of the most commonly used might include

| Fortran Type   | Module Name | C Type  |
|----------------|-------------|---------|
| INTEGER        | C_INT       | int     |
| INTEGER(int64) | C_INT64_T   | int64_t |
| REAL           | C_FLOAT     | float   |
| REAL(real64)   | C_DOUBLE    | double  |
| LOGICAL        | C_BOOL      | \_Bool  |
| CHARACTER      | C_CHAR      | char    |

If all the types can be matched, a C struct can be mapped to a Fortran type with the BIND(C) attribute.
```fortran
USE ISO_C_BINDING
 TYPE, BIND(C) :: myType
   INTEGER(C_INT) :: i, j
   REAL(C_DOUBLE) :: d
   CHARACTER(KIND=C_CHAR) :: c
 END TYPE
```
The KIND= keyword is required for CHARACTER because the default argument is the LEN.
This would correspond to a C struct
```C
struct {
   int i, j;
   double d;
   char c;
 } myType;
```
Some facts to keep in mind are that C arrays number from 0, and C strings are terminated with a NULL character (C_NULL_CHAR in the module).

Subprograms must also declare the BIND(C) attribute to set up C bindings.
Caution is required since C generally passes by _value_, which makes a copy, whereas Fortran effectively passes by _reference_, i.e. the address of the memory location that holds the variable.  Arrays must also receive special treatment. Pointers require particular care. 
For a very simple example, suppose we have a C function
```C
int adder(int i, int* j)
```
and we wish to write Fortran bindings to it.  The first variable is passed by value so we must add the VALUE attribute to its declaration in Fortran.  The second argument is passed by reference, as is the default in Fortran.  The Fortran declaration looks like
```fortran
 integer(c_int) function func(i,j)
    use iso_c_binding, only: c_int
    integer(c_int), VALUE :: i
    integer(c_int) :: j
```

A good general discussion of Fortran-C interoperability, from which the above example is taken, is from [gfortran](https://gcc.gnu.org/onlinedocs/gfortran/Interoperability-with-C.html#Interoperability-with-C).
