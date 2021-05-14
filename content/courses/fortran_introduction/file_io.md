---
title: "File IO"
toc: true
type: book
weight: 53

menu:
    fortran_introduction:
        parent: Input/Output
        weight: 53
---

Reading from and writing to the console works for simple programs, and is often used even in more complex codes to print error messages, progress indicators, and the like, but for most purposes we need to read from and write to files.  

A detailed overview of file IO can be found in Intel's [documentation](https://software.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/file-operation-i-o-statements.html).

## Open

A file must be _opened_ before it can be accessed by the executable.  In general, we associate some type of _file descriptor_ with the name of the file, then after making that connection, henceforth the file is referenced by its descriptor.

In Fortran, files are identified with integers called _unit numbers._ They are not generated automatically, but must be chosed by the programmer.
```fortran
OPEN(UNIT=iunit,FILE=fname)
```
The open command has many other options.  Only UNIT and FILE are required.  If the unit argument is first it does not need the "UNIT=" keyword.

On Unix file names will be _case sensitive_ .
In Unix unit 5 is conventionally standard input and unit 6 is standard output.  Standard error is not as uniform but it is usually unit 2.

Programmers can reassign units 2, 5, and 6, but it is strongly advised that you not do so.

### Commonly-Used Options to Open

```fortran
IOSTAT=ios
```
Returns status into the integer variable `ios`.  If the result is zero, the statement succeeded, otherwise it failed. Specific nonzero values are system-dependent.
```fortran
ERR=label
```
Jumps to the statement labeled `label` if an error occurs.
```fortran
END=label
```
Jumps to the statement labeled `label` on end of file.
```fortran
STATUS=stat
```
The value of `stat` can be `old`, `new` , `replace`, `scratch`, or  `unknown`.  The default is `unknown` (read/write permission).  If `old` it must exist, and if `new` it must not exist.  A `scratch` file is automatically deleted after being closed.
```fortran
position=pos
```
Position `pos` is `asis` (the default), `rewind`, or `append`.  Rewind returns the file pointer to the top, which will cause the file to be overwritten by new data.  Append leaves it at the end of the file so new data can be added.

```fortran
FORM=fmt
```
The permitted values for `fmt` are `formatted` (the default, for a text file) or`unformatted` (a system-dependent binary format).
```fortran
ACCESS=acc
```
The access `acc` can be `sequential` (the default), `direct`, or `stream`. Direct files must be unformatted.
Unless access is `stream`, an unformatted file will have a header and footer that is specific to a compiler and platform and may not be portable.  This is a relic of magnetic tape drives and allowed them to backspace and skip forward in the tape.  For a binary-format file similar to that produced by C/C++ programs and interchangeable with them, use
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
ERR=label      ! Likeopen
EXIST=exists   ! Returns.true.or.false.into logical variable exists
OPENED=is_open ! Returns.true.or.false.into logical variable is_open
```

## Close

Much of the time, it is not necessary to close a file explicitly.  Files are automatically closed when execution terminates.

If many files are opened, it is good practice to close them before the end of the run.
```fortran
CLOSE(UNIT=iunit,IOSTAT=ios,ERR=err,STATUS=stat)
```
STATUS can be `keep` (default) or `delete`.  UNIT, IOSTAT, and ERR are like the corresponding options to OPEN.

The most typical usage is simply to close the file.
```fortran
CLOSE(iunit)
```

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

## REWIND

An open unit can be rewound.  This places the _file pointer_ back to the beginning of the file.
The default is to rewind a file automatically when it is closed.
If you want to rewind the file to reread it, use
```fortran
REWIND(iunit)
```
REWIND is convenient if the program must handle files whose lengths may vary.  Read through the file without storing any variables, count the number of lines, rewind, then allocate any arrays needed.

If your input files will always be of known length this isn’t necessary (or efficient), but often file length could vary with different data.

Example
```fortran
   nlines=0
   do
      read(iunit,*,end=1)
      nlines=nlines+1
   end do
1  continue

   rewind(iunit)

   allocate(obs(nlines))

   do n=1,nlines
      read(iunit,*)obs(n)
   enddo
```

QUIZ

Why do I increment `nlines` _after_ the read?
What would I do if I had one or more header lines?

{{< spoiler text="Answer" >}}
I need to wait until the line has been successfully read before I count it. Consider an empty file (zero lines).  I will immediately encounter end of file, so I want to exit then.  For a one-line file, it reads the first line and I count that, then next time around it hits end of file and exits.  Nlines=1 then, which is correct.  The rest follows by induction.  For the second question, if I have header lines then I must use one READ per line to move through them before entering the loop to read data.  I can ignore the contents of the line if they are unneeded by providing no variable to store the data.
{{< /spoiler >}}

