---
title: "Characters and Strings"
toc: true
type: book
weight: 43

menu:
    fortran_introduction:
        parent: Character Variables
        weight: 43

---

Fortran's support of characters and strings has evolved significantly through the recent standards. The earliest Fortran used _Hollerith codes_ to represent characters.  The `character` type was introduced in the Fortran 77 standard. The original character type is essentially a fixed-length string.  Variable-length strings were introduced in the Fortran 2003 standard.  

## Character Sets and Encodings

Like everything else in the computer, characters must be represented by a sequence of 0s and 1s.  A catalogue of these representations is usually called an _encoding_.

The basic character set used by Fortran is [**ASCII**](https://en.wikipedia.org/wiki/ASCII), the American Standard Code for Information Interchange. Internationally, it is sometimes known as US-ASCII.  Originally, 7 bits were used to represent data, resulting in a total of 128 (2<sup>7</sup>) available characters.  The first 32 are _non-printing_ characters that were mainly needed by the mechanical devices for which the encoding was developed.  Only a few non-printing characters are still used, among them line feed, carriage return, and even "bell."  

Exercise:
```fortran
program ringer
    print *, char(7)
end program
```
ASCII now uses 8 bits (extended ASCII) which doubles the number of available characters, but the first 128 character codes are the same as 7-bit ASCII.  It was not as well standardized as ASCII, though standards exist for Latin alphabets (ISO 8859-1, ISO Latin 1) and Cyrillic (ISO 8859-5).  Nearly all programming languages continue to restrict the characters allowed for statements to the original ASCII, even when larger character sets are the default and may be used for comments and output.

Even extended ASCII accommodates far too few characters to accommodate more than a handful of alphabets, much less other writing systems.  [Unicode](https://en.wikipedia.org/wiki/Unicode) was created to address this.  The first 128 codes are still the 7-bit ASCII codes even with the millions available through Unicode.  Fortran supports a version of Unicode called [ISO_10646](https://en.wikipedia.org/wiki/Universal_Coded_Character_Set) for comments and output, though not all compilers implement it yet.

Example, from the gfortran [documentation](https://gcc.gnu.org/onlinedocs/gcc-4.9.4/gfortran/SELECTED_005fCHAR_005fKIND.html). It may compile only with gfortran.

{{< code file="courses/fortran_introduction/iso.f90" lang="fortran" >}}

## Fixed-Length Strings

Declare character variables with
```fortran
CHARACTER(len=<N>) :: string
```
The value of `N` must be an integer literal and it must be large enough to contain all characters in the string, including whitespace (tab character, space).

Arrays of character variables are permitted.
```fortran
CHARACTER(len=3), DIMENSION(10) :: string
```
This is a 10-element, rank-1 array, each of whose elements is a length-3 character.

## Variable-Length Strings

Variable-length strings are declared similarly to allocatable arrays.
```fortran
CHARACTER(len=:), ALLOCATABLE :: string
```
The string must be allocated in the executable code before it is used.
```fortran
   num_chars=5
   allocate(character(len=num_chars) :: string)
```

Allocatable arrays of allocatable strings are possible, but will require creating a [derived type](/courses/fortran_introduction/derived_types).

Prior to Fortran 2003, the standard defined a module `iso_varying_string`.  Most compilers available now support the 2003 standard so will offer the standard variable string, but the iso_varying_string module provides a number of functions so may still be worthwhile.  We will discuss standardized modules [later](courses/fortran_introduction/standard_modules).

## Substrings

Similar to arrays, slices or _substrings_ may be extracted from strings.  Characters are counted starting from 1 at the left and the upper bound of the range is included.
If the lower bound is omitted, the first character is assumed.  If the upper bound is omitted, characters from the specified character to the end of the string are included.  To extract a single character, the range is from its position to the same position.
```fortran
character(len=11) :: message

message="Hello world"
print *, message(1:5)," ",message(5:5)," ",message(7:)
```
This results in
```
 Hello o world
```

## Concatenation

The only string operator is concatenation `//`.  Try this out and see what
is printed.  Remember to add the `program`, `implicit none`, and `end program` statements to your program, and indent properly.
```fortran
character(len=5)  :: word1, word2
character(len=20) :: message

word1="Hello"
word2="world"
message=word1//" "//word2
print *, message
```
Now try
```fortran
print *, message//" today"
```
You should see
```
Hello world          today
```

### String Length
A useful string function, especially for variable-length strings, is `LEN(S)`.
A fixed-length string will always occupy the specified number of characters. The default is to left-justify nonblank characters in the field.  This can be modified with [intrinsics](courses/fortran_introduction/character_intrinsics).

**Exercises**
* Declare character variables large enough to hold the indicated strings.  Make full_title at least 5 characters longer than you think necessary.
```fortran
title="Jaws"
subtitle="The Revenge"
print *,len(title)
full_title=title//":"//subtitle
print *, full_title
print *,len(full_title)
print *,full_title(2:4)
```
   1. Change “Jaws” to “Paws” in full_title
   2. Make the strings variable sized.  Use the `len` function.

{{< spoiler text="Solution with variable strings." >}}
{{< code file="courses/fortran_introduction/solns/var_strings.f90" lang="fortran" >}}
{{< /spoiler >}}
