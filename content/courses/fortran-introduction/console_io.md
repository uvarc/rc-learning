---
title: "Console Input/Output"
date : "2021-04-5T00:00:00"
toc: true
type: book
weight: 51

menu:
    fortran-introduction:
        parent: Console Input/Output
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
READ _requires_ the unit identifier as the first option; here the `*` indicates console input. The second option must indicate how to format the output, with `*` telling the compiler to make its own choices.  READ can take additional options to check for errors and perform other housekeeping tasks.

```fortran
IOSTAT=ios 
```
Returns status into the integer variable `ios`.  If the result is zero, the statement succeeded, otherwise it failed. Specific nonzero values are system-dependent.
```fortran
IOMSG=msg 
```
For IOMSG the specific error message will vary by compiler, and no standard length is specified, but a declared length of 128 should be enough.  Use the `trim` intrinsic to print it more legibly.
```fortran
ERR=label   !Jump to label on error
END=label   !Jump to label on end of file 
EOR=label   !Jump to label on end-of-record (line) (nonadvancing READ only)
```

Fortran WRITE to the console:
```fortran
WRITE(*,*) var1,var2,var3
```
As for READ, the first option to WRITE is the unit identifier and the second is a format descriptor.  The asterisk as the format argument specifies a list-directed write in which the compiler formats the output based on its defaults.

WRITE has optional arguments for error checking similar to READ: IOSTAT, IOMSG, and ERR.  It also has some optional arguments to control certain aspect of the output appearance, particularly those related to differences in conventional representation of numbers, for example:
```fortran
DECIMAL=dec
```
where `dec` is a character variable or literal that evaluates to COMMA or POINT.  This controls whether floating-point numbers are printed with a comma or decimal point.

The PRINT statement always writes to the console (standard output for Unix).  The asterisk specifies list-directed IO.
```fortran
PRINT *, var1,var2,var3
```

In Fortran the PRINT statement always writes an end-of-line marker after all variables have been output.  The WRITE statement does as well, unless told otherwise. This is the _opposite_ of the behavior of `write` in most other languages.

**Example**

{{< code file="/courses/fortran-introduction/codes/consoleio.f90" lang="fortran" >}}

## Reading from the Command Line

Input values can be read from the command line.  This is usually accomplished in an IDE through an option to `Run`.

We can read strings only.  You must convert if necessary to a numerical type using internal read/write.  See the discussion [earlier](courses/fortran-introduction/operators).

The COMMAND_ARGUMENT_COUNT intrinsic returns the number of command-line options.  For each one, we must call GET_COMMAND_ARGUMENT with its number and a character buffer variable.
```fortran
   nargs=command_argument_count()
   if (nargs .ne. 1 ) then
      stop "No input specified"
   else
      call get_command_argument(1,nval)
      read(nval,'(i4)') n
      call get_command_argument(2,mval)
      read(mval,'(i4)') m
   endif
```

Example:
{{< code file="/courses/fortran-introduction/codes/clio.f90" lang="fortran" >}}

## Exercises

1. In an “infinite” while loop:
Request an integer from the user with non-advancing input/output, e.g.
“Please enter an integer:” <then read integer>
If the integer is 1, print “zebra”.  If it is 2, print “kangaroo”.  If it is anything else except for zero, print “not found”.  If it is 0, exit the loop.

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/fortran-introduction/solns/console_io.f90" lang="fortran" >}}
{{< /spoiler >}}

2. Write a program that takes a string as the command-line argument.  Print the string to standard output.  Use `trim` or any other string operators or function s to make the output neat.  If you read a string from the command line you do not have to do any conversion of the variable.

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/fortran-introduction/solns/command_line.f90" lang="fortran" >}}
{{< /spoiler >}}
