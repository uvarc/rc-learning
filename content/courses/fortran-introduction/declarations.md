---
title: "Variable Declarations"
date : "2021-04-05T00:00:00"
date : "2021-04-5T00:00:00"
toc: true
type: book
weight: 22

menu:
    fortran-introduction:
        parent: Basic Programming Constructs
        weight: 22

---

Like most compiled languages, Fortran is _statically_  _typed_ .  All variables must be _declared_ to be of a specific type before they can be used.  A variable’s type cannot be changed once it is declared.

Fortran is (nearly) strongly typed.  Mixed-mode expressions are limited and most conversions must be explicit.

Unlike most languages, Fortran is _not_ case-sensitive.  Variables `Mean`, `mean`, and even `mEan` are the same to the compiler.

Variable names may consist of alphanumeric (letter or digit) characters, plus underscores.  No other characters, including spaces, are permitted.  The first character must be an alphabetical character.  The maximum length of a variable name for modern Fortran as of Fortran 95 is 31 characters.  The 6-character limit of Fortran 77 is long gone.  Some compilers permit up to 127 characters as an extension, though excessively long variable names is _not_ a good programming practice.  

A good descriptive variable name often consists of multiple words or parts of words.  Since Fortran is not case-sensitive, underscores can be used to separate the components.
```fortran
is_valid
start_date
num_species
```
Separation through capitalization is possible with the understanding that different variables cannot be distinguished by "camel case."
```fortran
subroutine BioGeoChem
type myType
```

Variables are declared by indicating the type followed by a comma-separated list of variables.
In older code no separator was used.
```fortran
INTEGER i, j, k
```
In newer code, use the double colon to separate the type from the variable list
```fortran
INTEGER  :: i, j, k
```
If there are other attributes on the line the :: will be _required_ .

It is _not_ necessary to write keywords, or any source at all, in all capital letters, but they may be written in capitals here for clarity.

## Declarations by Type

{{< table >}}
|   Fortran Name   |    Type   |   Standard?       |
|------------------|-----------|----------------------------|
|     INTEGER      |  32-bit integer |  Yes                  |
|     INTEGER\*8   |  64-bit integer |  No, but nearly universal |
|     INTEGER(ik)  |  Integer specified by KIND |  Yes |
|     REAL         |  Single precision floating point | Yes  |
| DOUBLE PRECISION |  Double precision floating point | Yes, but deprecated style |
| REAL\*8 |  Double precision floating point |  No, but universal |
| REAL(rk)|  Floating point denoted by KIND |  Yes |
| LOGICAL |  Logical (Boolean)  |  Yes |
| COMPLEX  |  Single precision complex  | Yes |
| COMPLEX\*8 |  Double precision complex  | No, but nearly universal |
| CHARACTER  |  One character  | Yes |
| CHARACTER(LEN=10)  | Character variable with 10 characters | Yes |
| CHARACTER\*10  | Character variable with 10 characters | Yes, but deprecated style |
|     BYTE         |  One byte  | Yes  |
{{< /table >}}

Other types may be specified through [KIND].

## Implicit and Explicit Typing

For historical reasons, Fortran used _implicit typing_ for numerical types.  Any variable starting with the letters A-H or O-Z were _floating point_.  Variables beginning with the letters I-N were _integers_.  Note that `IN` are the first two letters of the word "integer."  That is a longstanding mathematical tradition and Fortran was developed to translate mathematical notation (FORmula TRANslation).

Older code often changes the default float to double:
```fortran
IMPLICIT DOUBLE PRECISION(a-h,o-z)
```
However, in modern usage, _all_ variables should be explicitly typed.  This will enable the compiler to catch typographical errors.  If implicit typing is used, a new variable would not need to be declared and would assume the type based on its name, so a misspelling of an existing variable would create a different variable.  Bugs like this can be difficult to track down.

The statement
```fortran
IMPLICIT NONE
```
negates implicit typing.  It must be the first line after a unit declaration unless a [USE](/courses/fortran-introduction/modules) is present.

**Example**
```fortran
PROGRAM simple
IMPLICIT NONE
INTEGER              ::   I, J
REAL                 ::   R, S, T
DOUBLE PRECISION     ::   D
DOUBLE COMPLEX       ::   Z
LOGICAL              ::   FLAG
CHARACTER (len=20)   ::   C
```
Line up declarations neatly.

## Initializing at Compile Time

Variables can be declared and initialized at the same time:
```fortran
real  :: x=1.e-8, y=42.
```
When variables are initialized in this manner it happens only _once_ , at compile time.  If this takes place in a subprogram it will not happen again upon repeated invocations.

It is equivalent to the older DATA statement:
```fortran
DATA x,y/1.e-8,42./
```

In Fortran 2003 it became possible to initialize using intrinsic functions:
```fortran
real  :: pi = 4.0*atan(1.0)
```

**Example**

Start your choice of IDE or editor.  Type
```fortran
program first
! My first program
! Author:  Your Name
  implicit none
  real     ::x,y
  integer  ::i,j=11
     x=1.0
     y=2.0
     i=j+2
     print *, "Reals are ",x,y
     print *, "Integers are ",i,j
end program
```

### PARAMETER

In compiled languages, programmers can declare a variable to have a fixed value that cannot be changed.
In Fortran this is indicated by the `PARAMETER` attribute.
```fortran
REAL, PARAMETER  ::  pi=4.0*ATAN(1.0)
```
Attempting to change the value of a variable declared to be a parameter will result in a fatal compiler error.

In older code the declaration and parameter statement will be on different lines
```fortran
real  pi
parameter (pi=3.14159)
```
