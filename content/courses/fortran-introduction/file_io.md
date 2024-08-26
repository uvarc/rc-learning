---
title: "File IO"
date : "2021-04-5T00:00:00-05:00"
toc: true
type: book
weight: 53

menu:
    fortran-introduction:
        parent: File IO
        weight: 53
---

Reading from and writing to the console works for simple programs, and is often used even in more complex codes to print error messages, progress indicators, and the like, but for most purposes we need to read from and write to files.  

## Open

A file must be _opened_ before it can be accessed by the executable.  In general, we associate some type of _file descriptor_ with the name of the file, then after making that connection, henceforth the file is referenced by its descriptor.

In Fortran, files are identified with integers called _unit numbers._ They are not generated automatically, but must be chosen by the programmer.
```fortran
OPEN(UNIT=iunit,FILE=fname)
```
The open command has many other options.  Only UNIT and FILE are required.  If the unit argument is first it does not need the "UNIT=" keyword.

On Unix file names will be _case-sensitive_ .
In Unix unit 5 is conventionally standard input and unit 6 is standard output.  Standard error is not as uniform, but it is usually unit 2.

Programmers can reassign units 2, 5, and 6, but it is strongly advised that you not do so.

## Close

Much of the time, it is not necessary to close a file explicitly.  Files are automatically closed when execution terminates.

If many files are opened, it is good practice to close them before the end of the run.
```fortran
CLOSE(UNIT=iunit)
```
If you wish to reopen a file for some reason, you must first CLOSE it.

## Read/Write with Files

The file must first be opened and a unit assigned.

```
READ(iunit,*)
```
List-directed output is indicated by an asterisk.  Formatted output requires a format string, or a reference to a labeled FORMAT statement.
```
WRITE(iunit,*)
WRITE(iunit,'(fmtstr)')
```
or
```
      WRITE(iunit,label)
label FORMAT(fmtstr)
```
If the unit identifier is not the first in the list it must be written as `UNIT=iunit`.  The `UNIT=` keyword is optional otherwise.

## NAMELIST

One of the most convenient I/O statements in Fortran is NAMELIST.  With this statement, parameters in an input file can be specified by `name=value` pairs and in any order.

The namelist must be declared.  This is a non-executable statement.  The syntax is:
```fortran
NAMELIST /name/ var1,var2,var3
```
The name is chosen by the programmer.
The namelist is read with a special form of the READ statement.
```fortran
read(iunit, name)
```

## Namelist Input

The input file containing the namelist must follow a specific format. Namelist was not part of the Fortran 77 standard so there is some variation.  However, thenamelistalways starts with
```fortran
&name
```
The variable list follows, with each variable on a separate line and consisting of the varname=value pair.
In older code, the namelist frequently terminates with another ampersand `&`, 
or `&end`.  Also, in Fortran 77 there may be rules about in which column the `&` can occur.

Namelist was established as part of the standard in Fortran 90. According to the standard, the namelist is terminated with a forward slash `/`.

**Example**
In the program
```fortran
NAMELIST /params/ rho,eps, x0

OPEN(10,file='paramlist.txt')
READ(10,params)
```
The input file (Fortran 90 format) would be
```fortran
&params
rho=1.3
eps=1.e-7
x0=0.0
/
```

**Exercises**

1. Write a program that creates a file mydata.txt containing four rows consisting of
```
1, 2, 3
4, 5, 6
7, 8, 9
10, 11, 12
```
Close the file, then open it again.  Read the data back.  Write a loop to add 1 to each value and print each row to the console.

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/fortran-introduction/solns/reopen.f90" lang="fortran" >}}
{{< /spoiler >}}

2. Write a program that reads the `params` namelist and prints the variables to the console.  Create the paramlist.txt file and test your program.

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/fortran-introduction/solns/nlist.f90" lang="fortran" >}}
{{< /spoiler >}}
