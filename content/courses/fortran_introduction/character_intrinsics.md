---
title: "Character Intrinsics and Conditionals"
toc: true
type: book
weight: 44

menu:
    fortran_introduction:
        parent: Character Variables
        weight: 44

---

## Character Functions

These intrinsic functions operate on individual characters.

```fortran
! Returns the integer in the character sequence corresponding to the single
! character c
ICHAR(C [,KIND])
! Same as ichar
IACHAR(C [,KIND])
! Returns the character corresponding to integer i
CHAR(I [,KIND])
! Same as char
ACHAR(I [,KIND])
```

## String Functions

These intrinsic functions operate on strings of any length.

### Length and Trimming

```fortran
! Length of string (returns declared size for fixed-length strings)
LEN(STR)
! Length of string with trailing blanks removed
LEN_TRIM(STR)
! Returns new string with trailing blanks removed from argument
TRIM(STR)
! Concatenate multiple copies of the same string
REPEAT(STR,NCOPIES)
```

### Alignment

These functions adjust the appearance of a string.

```fortran
! Adjust left by removing leading spaces
ADJUSTL(STR)
! Adjust right by removing trailing spaces. Pad front if necessary.
ADJUSTR(STR)
```
Example
```fortran
character(len=15) :: lang
lang="Fortran"
print *, lang
print *, adjustl(lang)
print *, adjustr(lang)
```
The default is to left justify the string.

### Searching

Search for characters and substrings.

```fortran
! In these, the BACK option, if present, must be logical.
! Return starting indext of SUBSTR from left (from right with BACK)
!   or zero if SUBSTR not present.
INDEX(STR, SUBSTR[, BACK [, KIND]])
! If any of SET is in STR, return leftmost (rightmost with BACK) position in STR
SCAN(STR, SET[, BACK [, KIND]])
! Check whether all the characters of SET are in STR, return first location 
   of a character _not_ in SET, or zero if all are present.
! VERIFY(STR, SET[, BACK [, KIND]])
```
Example
```fortran
character(len=15) :: lang
character(len=5) :: ort
lang="Fortran"
ort="ort"
print *, index(lang,'f')
print *, index(lang,'F')
print *, scan(lang, ort)
print *, verify(lang, ort)
```

## Character Comparison Operators

Strings can be compared to one another. 
These operators use _lexical ordering_, which is based on the ordering in the _character set_ along with rules for comparing multiple-character strings one 
character at a time.  The usual operators `==`,`/=`,`<`,`<=`,`>`,`>=` may be used, as well as the functions below.  The standard operators use ASCII ordering, whereas the functions use the character set on a particular platform, which not use ASCII ordering.

String comparisons _are_ case sensitive.

```fortran
! Returns .true. if stringA is lexically greater than or equal to stringB, 
!   otherwise returns .false.
LGE(STRINGA,STRINGB)
! Lexically greater than to stringB, 
LGT(STRINGA,STRINGB)
! Returns .true. if stringA is lexically less than or equal to stringB, 
!  otherwise returns.false.
LLE(STRINGA,STRINGB)
! Lexically less than string B 
LLT(STRINGA,STRINGB)
```
Example
```fortran
print *, lang=="Fortran"
print *, lang=="fortran"
print *, "Fortran"<"fortran"  !surprise!
print *, "2.">="2.0" 
print *, lle("2.","2.0") 
print *, "1"<="10"  
print *, llt("1","10")  
print *, "1"<=" 1"  
```
