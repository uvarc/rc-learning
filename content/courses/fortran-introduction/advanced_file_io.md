---
title: "Advanced File IO"
date : "2021-04-5T00:00:00"
toc: true
type: book
weight: 55

menu:
    fortran-introduction:
        parent: Advanced File IO
        weight: 55
---
The basic file input/output commands previously covered are sufficient for many programs, but far more control is possible through the use of other commands and options.  We will describe on the most common here.  
A detailed overview of file IO can be found in Intel's [documentation](https://software.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/file-operation-i-o-statements.html).

## OPEN Options

Several options to OPEN are related to error checking and are common among several file IO commands:
```fortran
IOSTAT=ios !Returns status into integer variable ios.  Nonzero value for failure.
IOMSG=iomesg !Returns into a character variable msg an informative message on error
ERR=label  !Jumps to the statement labeled `label` if an error occurs.
END=label  !Jumps to the statement labeled `label` on end of file.
```
The following are specific to OPEN and describe the file type:
```fortran
STATUS=stat
```
The value of `stat` can be 'OLD', 'NEW' , 'REPLACE', 'SCRATCH', or  'UNKNOWN'.  (As usual, the strings are not case-sensitive.)  The default is 'UNKNOWN' (read/write permission).  If 'OLD' it must exist, and if 'NEW' it must not exist.  A 'SCRATCH' file is automatically deleted after being closed.
```fortran
POSITION=pos
```
Position `pos` is 'ASIS' (the default), 'REWIND', or 'APPEND'.  REWIND returns the file pointer to the top, which will cause the file to be overwritten by new data.  APPEND leaves it at the end of the file so new data can be added.

```fortran
FORM=fmt
```
The permitted values for `fmt` are 'FORMATTED' (the default, for a text file) or 'UNFORMATTED' (a system-dependent binary format). This term is not related to whether the text is "formatted" for printing or not.
```fortran
ACCESS=acc
```
The access `acc` can be 'SEQUENTIAL' (the default), 'DIRECT', or 'STREAM'. Direct files must be unformatted.
Unless access is 'STREAM', an unformatted file will have a header and footer that is specific to a compiler and platform and may not be portable.  This is a relic of magnetic tape drives and allowed them to backspace and skip forward in the tape.  For a binary-format file similar to that produced by C/C++ programs and interchangeable with them, use
```fortran
OPEN(UNIT=iunit,FILE=fname,ACCESS=stream,FORM=unformatted)
```

## Inquire

The INQUIRE statement tests the status of a file.  Most usually we wish to check whether the file exists, or is already open, before we attempt to open it.
```fortran
INQUIRE(UNIT=iunit,options)
```
or
```fortran
INQUIRE(FILE=fname,options)
```
So we can inquire by unit or name but not both.

### Common Options to Inquire

Several of the options to INQUIRE are similar to those of OPEN.  Others are more specific.

```fortran
IOSTAT=ios     ! Like open, ios must be integer
ERR=label      ! Like open
EXIST=exists   ! Returns .true. or .false. into logical variable exists
OPENED=is_open ! Returns .true. or .false. into logical variable is_open
```

## READ Options for Files

The options to READ and WRITE include those already described for console IO, as well as some that are only relevant to files.  As an example, for ACCESS=STREAM files only, the position specifier can be used to start reading from a particular location on the file.
```fortran
POS=p
```
See [documentation](https://software.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/a-to-z-reference/q-to-r/read-statement.html#read-statement) for more options.

## CLOSE

The complete form of the CLOSE command is
```fortran
CLOSE(UNIT=iunit,IOSTAT=ios,ERR=err,STATUS=stat,IOMSG=iomesg)
```
STATUS can be 'KEEP' (default) or 'DELETE'.  UNIT, IOSTAT, IOMSG, and ERR are like the corresponding options to OPEN.

## REWIND

An open unit can be rewound.  This places the _file pointer_ back to the beginning of the file.
The default is to rewind a file automatically when it is closed.
If you want to rewind the file to reread it, use
```fortran
REWIND(iunit)
```
REWIND is convenient if the program must handle files whose lengths may vary.  Read through the file without storing any variables, count the number of lines, rewind, then allocate any arrays needed.

If your input files will always be of known length this isn’t necessary (or efficient), but often file length could vary with different data.

**Example**

{{< code file="/courses/fortran-introduction/codes/fileio.f90" lang="fortran" >}}

QUIZ

Why do I increment `nlines` _after_ the read?
What would I do if I had one or more header lines?

{{< spoiler text="Answer" >}}
I need to wait until the line has been successfully read before I count it. Consider an empty file (zero lines).  I will immediately encounter end of file, so I want to exit then.  For a one-line file, it reads the first line and I count that, then next time around it hits end of file and exits.  Nlines=1 then, which is correct.  The rest follows by induction.  For the second question, if I have header lines then I must use one READ per line to move through them before entering the loop to read data.  I can ignore the contents of the line if they are unneeded by providing no variable to store the data.
{{< /spoiler >}}

**Exercise**

Read the mydata.txt file but don't assume you know how long it is.  Inquire whether it is present, then read it through just to count the number of lines.  Rewind the file to read the data.  

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/fortran-introduction/solns/rewind.f90" lang="fortran" >}}
{{< /spoiler >}}
