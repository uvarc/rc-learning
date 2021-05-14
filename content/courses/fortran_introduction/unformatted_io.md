---
title: "Console Input/Output"
toc: true
type: book
weight: 51

menu:
    fortran_introduction:
        parent: Input/Output
        weight: 51
---

Programs that cannot read input data and write their results to some medium are
of little use.  We have used `print *` but there is much more to input/output.

## Console Input/Output

Most operating systems have some type of _console_.  Output written to the console appears as text on a terminal or equivalent.  Geany opens a window for console output when a program is executed.

The console in Fortran can be represented with an asterisk `*`.  In Unix this corresponds to the standard input stream for reading, and the standard output for writing.  For standard error a file must be associated with it.

Input/output commands can be _list directed_, where
the compiler handles the spacing and other aspects of the appearance of the output, or the programmer can explicitly _format_ the output.

## List-Directed Read/Write from/to the Console

Fortran read from the console.  Values may be separated by commas _or_ whitespace:
```fortran
READ(*,*) var1, var2, var3
```
Fortran write to the console:
```fortran
PRINT *, var1,var2,var3
WRITE(*,*) var1,var2,var3
```
In Fortran the PRINT statement always writes an end-of-line marker after all variables have been output.  The WRITE statement does as well, unless told otherwise. This is the opposite of write in most other languages.

## Reading from the Command Line

Input values can be read from the command line.  This is usually accomplished in an IDE through an option to `Run`.

We can read strings only.  You must convert if necessary to a numerical type using internal read/write.  See the discussion [earlier](courses/fortran_introduction/types).

The COMMAND_ARGUMENT_COUNT intrinsic returns the number of command-line options.  For each one, we must call GET_COMMAND_ARGUMENT with its number and a character buffer variable.
```fortran
   nargs=command_argument_count()
   if (nargs.ne. 1 ) then
      stop "No input specified"
   else
      call get_command_argument(1,nval)
      read(nval,'(i4)') n
      call get_command_argument(2,mval)
      read(mval,'(i4)') m
   endif
```

# Exercise

 Write a program that computes pi using a trig identity such `asp=4*atan(1)`
 Use kind to switch between real and double precision
  * integer, parameter ::rk=kind(1.0)  or (1.0d0)
* Using single precision, print pi in
  * E format
  * Scientific notation
  * Scientific notation with 8 decimal places
* Repeat for double precision

In an “infinite” while loop:
Request an integer from the user with non-advancing input/output, e.g.
“Please enter an integer:” <then read integer>
If the integer is 1, print “zebra”.  If it is 2, print “kangaroo”.  If it is anything else except for zero, print “not found”.  If it is 0, exit the loop.

## Formatted Input/Output

List-directed IO is convenient.  
But that frequently results in sprawling output that is difficult to read. Formatted output is frequently required for legibility.  

Formatted output in Fortran is similar to other languages (the general layout descends from Fortran, the oldest higher-level programming language).

In Fortran it is best to avoid formatted _input_ as much as possible, as it can lead to errors.

## Edit Descriptors

The edit descriptor modifies how to output the variables.  They are combined into forms like
```
RaF.w
```
where `R` is a repeat count, `a` is the descriptor, `F` is the total field width _including_ space for `+-`, and if requested `+-e` and exponent, and `w` is the number of digits to the right of the decimal point. If you are willing to let the compiler calculate the number of characters to use, then `Ra.w` alone works.  For floating-point numbers, `Ra.0` will print the integer part.

Strings take only `RaF` and do not usually require the `F` since the length will be known to the compiler.

Integers can be written as `iF` and any of the `F` spaces not needed will be blank filled, with the digits right justified.  When written as `iF.m` they will be printed with at least `m` digits and the rest of the field zero-filled on the left if all of `F` is not needed.

If the field width is specified and the requested literal does not fit, the compiler will output a string of asterisks, e.g. `********`.

### Common Edit Descriptors

As usual, they are not case sensitive.
```fortran
I  !integer
F  !real (decimal output)
E  !real (exponential output)
G  !general 
D  !double precision (prints D rather than E for exponent)
A  !character (does not require a field width in most cases)
X  !space
```
The real descriptors `F`, `E`, `G`, and `D` all work for both single and double precision. `G` allows the compiler to choose whether to use decimal or exponential format.

### Modifiers

Some modifiers can change the appearance of the output.
```fortran
p  !multiply by 10
kp !multiply by 10k
/  !write an EOL and go to the next line (record) within the format
```
Applies till the next scale factor is encountered.
```fortran
ES !use scientific notation.  
```
The default exponential format writes in machine normalization, with the leading digit between 0 and 1. `ES` causes it to write with the leading digit between 1 and 9, which is what most humans can read most easily.  Ignores `p` on output.

## Format Strings

The format string is constructed as a list of how to output the variables.  Unlike some other languages, literal strings are never included, but must have their own edit descriptors.
The format string can be placed directly into the `write` statement or it can be in a separate `format` statement. In the `write` it is enclosed in parentheses and quotes.
For most purposes it is best to put the format string into the write statement.  The format statement is older and will be in old code, but it is usually harder to see what is happening.  It is useful for particularly long strings, however.

# Examples
```
write(*,'(i5,2x,i6)') i1,i2
write(*,'(i5,a,i6)')) i1,"  ",i2
write(*,'(a,i4,es15.7)') "row",n,var
write(*,'(3(i2,3x,f8.3)') (r(j),var(j),j=1,3)
write(*,'(2f8.2)') z !complex
write(*,'(2L)')is_zero,is_finite
write(*,'(2p,f8.2,0p,f8.2)') var1, var2
write(*,'(a,f8.2,/,a,i6)') mess1,x,mess2,i
```

## Format Statements

Format statements are abundant in older code, before the strings could be inserted into writes.
FORMAT is non-executable but can appear anywhere in the source.  It is the only non-executable statement that can do so.
It can still be useful for a particularly complex format (to keep the write statement short and readable) or for formats that are repeated in many write statements.
The second parameter to the write is then an integer statement label.  The label marks the format statement.

Format Example
```fortran
    write(*,100)x,y,z
100 format(3e15.8)
```

Traditionally the format is placed immediately below the line which refers to it, or else all format statements are grouped together just before the end statement of their program unit.

# Fortran Non-Advancing IO

* If we’d like to write to and read from standard input on the same line we can use non-advancing IO:
* write(*,'(a)',advance='no') "Enter input value:"
* read(*,*) value
* _Must_ be formatted
  * ‘yes’ for advance is valid also but is the default.
  * Argument toadvancecan be a character variable so you can decide based on conditionals to advance or not.
* If you do _not_ want to advance, useadvance='no'

# Exercise

* Write a program that computes pi using a trig identity such asp=4*atan(1)
* Use kind to switch between real and double precision
  * integer, parameter ::rk=kind(1.0)  or (1.0d0)
* Using single precision, print pi in
  * E format
  * Scientific notation
  * Scientific notation with 8 decimal places
* Repeat for double precision

In an “infinite” while loop:

Request an integer from the user with non-advancing input/output, e.g.

“Please enter an integer:” <then read integer>

If the integer is 1, print “zebra”.  If it is 2, print “kangaroo”.  If it is anything else except for zero, print “not found”.  If it is 0, exit the loop.


What would I do if I had one or more header lines?

# Reading from a File

* READ(iunit,*)
  * For the most part I do not recommend formatted input, but if it is required it is
  * READ(iunit,'(fmtstr)'))
  * or
  * READ(iunit,label)
  * label FORMAT(fmtstr)
* EachREADstatement reads one line (unless you provide a format string and insert a forward slash).

# Writing to a File

* WRITE(iunit,*)
  * List IO
* WRITE(iunit,'(fmtstr)')
* or
* WRITE(iunit,label)
* label FORMAT(fmtstr)
* labelmust be an integer
* Fortran always writes an EOL for eachWRITEstatement; you may add more with the/character but there is seldom a need for this.

