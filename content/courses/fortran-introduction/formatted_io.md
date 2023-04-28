---
title: "Formatted Input/Output"
toc: true
type: book
weight: 52

menu:
    fortran-introduction:
        parent: Input/Output
        weight: 52
---

List-directed IO is convenient.  
But that frequently results in sprawling output that is difficult to read. Formatted output is frequently required for legibility.  

Formatted output in Fortran is similar to other languages (the general layout descends from Fortran, the oldest higher-level programming language).

## Edit Descriptors

The edit descriptor modifies how to output the variables.  They are combined into forms like
```
RaF.w
```
where `R` is a repeat count, `a` is the descriptor, `F` is the total field width _including_ space for `+-`, and if requested `+-e` and exponent, and `w` is the number of digits to the right of the decimal point. If you are willing to let the compiler calculate the number of characters to use, then `Ra0.w` alone works.  For floating-point numbers, `Ra.0` will print the integer part.

Strings take only `RaF` and do not usually require the `F` since the length will be known to the compiler.

Integers can be written as `iF` and any of the `F` spaces not needed will be blank filled, with the digits right justified.  When written as `iF.m` they will be printed with at least `m` digits and the rest of the field zero-filled on the left if all of `F` is not needed.

If the field width is specified and the requested literal does not fit, the compiler will output a string of asterisks, e.g. `********`.

### Common Edit Descriptors

As usual, they are not case sensitive.
```fortran
I  !integer
F  !real (decimal output)
E  !real (exponential output)
ES !like E but use scientific notation.  
G  !general 
D  !double precision (prints D rather than E for exponent)
A  !character (does not require a field width in most cases)
X  !space
/  !write an EOL and go to the next line (record) within the format
```
The real descriptors `F`, `E`, `G`, and `D` all work for both single and double precision. `G` allows the compiler to choose whether to use decimal or exponential format.
The default exponential format writes in machine normalization, with the leading digit between 0 and 1. `ES` causes it to write with the leading digit between 1 and 9, which is what most humans can read most easily.  `ES` ignores `p` on output.

### Modifiers

Some modifiers can change the appearance of the output.
```fortran
p  !multiply by 10
kp !multiply by 10k
```
The `p` descriptor applies till the next scale factor is encountered.

Fill an integer field with zeros
```fortran
I4.4
```

## Format Strings

The format string is constructed as a list of how to output the variables.  Unlike some other languages, literal strings are never included, but must have their own edit descriptors.
The format string can be placed directly into the `write` statement or it can be in a separate `format` statement. In the `write` it is enclosed in parentheses and quotes.
For most purposes it is best to put the format string into the write statement.  The format statement is older and will be in old code, but it is usually harder to see what is happening.  It is useful for particularly long strings, however.

**Examples**
```fortran
write(*,'(i5,2x,i6)') i1,i2
write(*,'(i5,a,i6)')) i1,"  ",i2
write(*,'(a,f0.6)')) "The result is  ",res
write(*,'(a,i4,es15.7)') "row",n,var
write(*,'(3(i2,3x,f8.3)') (r(j),var(j),j=1,3)
write(*,'(2f8.2)') z !complex
write(*,'(2L)')is_zero,is_finite
write(*,'(2p,f8.2,0p,f8.2)') var1, var2
write(*,'(a,f8.2,/,a,i6)') mess1,x,mess2,i
```
A format string may be a variable
```fortran
character(len=32) :: formatstr
   code
   formt='(f8.3,es15.7)'
   write(*,formatstr) A, B
```

## Format Statements

Format statements are abundant in older code, before the strings could be inserted into writes.
FORMAT is non-executable but can appear anywhere in the source.  It is the only non-executable statement that can do so.
It can still be useful for a particularly complex format (to keep the write statement short and readable) or for formats that are repeated in many write statements.
The second parameter to the write is then an integer statement label.  The label marks the format statement.

**Example**
```fortran
    write(*,100)x,y,z
100 format(3e15.8)
```

Traditionally the format is placed immediately below the line which refers to it, or else all format statements are grouped together just before the end statement of their program unit.

## Formatted Input

In Fortran it is best to avoid formatted _input_ as much as possible, as it can lead to errors.  For historical reasons, if a format is specified to be real (floating point) but no decimal point is included in the data, the compiler inserts it based on the format.  For example, suppose we had data in the form
```no-highlight
112 9876 12
```
with a format string of
```fortran
read(infile,'(f8.4,f6.2,i4)'
```
We intended to read three values, two reals and an integer, but this format results in input values of
```no-highlight
      112.987602      0.119999997               0
```
The errors in the real values are the consequence of converting from decimal to binary and back.  
Note that the spaces between the values are ignored.

We get the expected result with `read(infile,*)`:
```no-highlight 
   112.000000       9876.00000              12
```
The compiler has more freedom so the conversion is also more accurate.

Unformatted input is permitted when casting from a character to a real, so there is no need for formatted input at all.
```fortran
character(len=12) :: quantity_char
real              :: quantity
quantity_char="100"
read(quantity_char,*) quantity
```

## Fortran Non-Advancing IO

* If we’d like to write to and read from standard input on the same line we can use non-advancing IO:
```fortran
write(*,'(a)',advance='no') "Enter input value:"
read(*,*) value
```
Non-advancing IO _must_ be formatted
  * ‘yes’ for advance is valid also but is the default.
  * The argument to `advance` can be a character variable so you can decide based on conditionals to advance or not.
  * If you do _not_ want to advance, use `advance='no'`

**Exercises**

1. Write a program that computes pi using a trig identity such as `pi=4*atan(1)`.
   * Use kind to switch between real and double precision
     * integer, parameter ::rk=kind(1.0)  or (1.0d0)
   * Using single precision, print pi in
     * E format
     * Scientific notation
     * Scientific notation with 8 decimal places
Repeat for double precision.

{{< spoiler text="Solution with variable strings." >}}
{{< code file="courses/fortran-introduction/solns/print_pi.f90" lang="fortran" >}}
{{< /spoiler >}}


2. In an “infinite” while loop:
 Request an integer from the user with non-advancing input/output, e.g.
```
"Please enter an integer:" <then read integer>
```
If the integer is 1, print "zebra".  If it is 2, print "kangaroo".  If it is anything else other than zero, print "not found".  If it is 0, exit the loop. This requires only a simple change to a previous program.

{{< spoiler text="Solution with variable strings." >}}
{{< code file="courses/fortran-introduction/solns/non_advance.f90" lang="fortran" >}}
{{< /spoiler >}}

