---
title: "Input/Output"
toc: true
type: docs
weight: 50

menu:
    fortran_introduction:
        name: Input/Output
        weight: 50
---

# List-Directed IO

* List-directed IO allows the compiler to format the data.
* Input
  * Fortran read from standard input.  Separates values on comma _or_ whitespace.
  * read(*,*) var1, var2, var3
  * Fortran write to standard output
  * print *, var1,var2,var3
  * write(*,*) var1,var2,var3
* In Fortran theprintstatement always writes an EOL after all variables have been output.  Thewritestatement does as well unless told otherwise (this is the opposite ofwritein most other languages).

# Reading from the Command Line

We can read strings only.  You must convert if necessary to a numerical type using internal read/write.

nargs=command_argument_count()

if (nargs.ne. 1 ) then

stop "No input specified"

else

callget_command_argument(1,nval)

read(nval,'(i4)') n

endif

# Formatted IO

In Fortran it is best to avoid formatted _input_ as much as possible, as it can lead to errors.

Formatted output, on the other hand, is frequently required for legibility.  Compilers tend to let list-directed output sprawl.

Formatted output is similar to other languages (in fact, they all got it from Fortran, the oldest higher-level programming language).

# Edit Descriptors

The edit descriptor modifies how to output the variables.  They are combined into forms like

RaF.w

where R is a repeat count,ais the descriptor,Fis   the total field width _including_ space for+-, and if requested+-eand exponent, andwis the number of digits to the right of the decimal point.Ra.walone works and Ra.0 will print the integer part.

Strings take onlyF(RaF) and do not usually require the F since the length will be known to the compiler.

Integers can be written asiFand any of theFspaces not needed will be blank filled, with the digits right justified.  When written asiF.mthey will be printed with at leastmdigits and the rest of the field zero-filled on the left if all ofFis not needed.

# Common Edit Descriptors

* As usual, they are not case sensitive.
  * Iinteger
  * Freal (decimal output)
  * Ereal (exponential output)
  * Ggeneral
  * Ddouble precision (printsDrather thanEfor exponent)
  * Acharacter (does not require a field width in most cases)
  * Xspace
* The real descriptorsF,E,G, andDall work for both single and double precision.Gallows the compiler to choose whether to use decimal or exponential format.

# Modifiers

Some modifiers can change the appearance of the output.

pmultiply by 10.kpmultiply by 10k. Applies till the next scale factor is encountered.

ESuse scientific notation.  The default exponential format writes in machine normalization, with the leading digit between 0 and 1.EScauses it to write with the leading digit between 1 and 9, which is what most humans can read most easily.  Ignorespon output.

/write an EOL and go to the next line (record) within the format

# Format Strings

The format string is constructed as a list of how to output the variables.  Unlike some other languages, literal strings are never included, but must have their own edit descriptors.

The format string can be placed directly into thewritestatement or it can be in a separateformatstatement. In thewriteit is enclosed in parentheses and quotes.

For most purposes it is best to put the format string into the write statement.  The format statement is older and will be in old code, but it is usually harder to see what is happening.  It is useful for particularly long strings, however.

# Examples

  * write(*,'(i5,2x,i6)') i1,i2
  * write(*,'(i5,a,i6)')) i1,"  ",i2
  * write(*,'(a,i4,es15.7)') "row",n,var
  * write(*,'(3(i2,3x,f8.3)') (r(j),var(j),j=1,3)
  * write(*,'(2f8.2)') z !complex
  * write(*,'(2L)')is_zero,is_finite
  * write(*,'(2p,f8.2,0p,f8.2)') var1, var2
  * write(*,'(a,f8.2,/,a,i6)') mess1,x,mess2,i

# Format Statements

Format statements are abundant in older code, before the strings could be inserted into writes.

FORMATis non-executable but can appear anywhere in the source.  It is the only non-executable statement that can do so.

It can still be useful for a particularly complex format (to keep the write statement short and readable) or for formats that are repeated in many write statements.

The second parameter to the write is then an integer statement label.  The label marks the format statement.

# Format Example

write(*,100)x,y,z

100 format(3e15.8)

Traditionally the format is placed immediately below the line which refers to it, or else all format statements are grouped together just before the end statement of their program unit.

# Fortran Non-Advancing IO

* If we’d like to write to and read from standard input on the same line we can use non-advancing IO:
* write(*,'(a)',advance='no') "Enter input value:"
* read(*,*) value
* _Must_ be formatted
  * ‘yes’ for advance is valid also but is the default.
  * Argument toadvancecan be a character variable so you can decide based on conditionals to advance or not.

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

# Fortran file io

# Open

Files are identified with integers called _unit numbers._

open(unit=iunit,file=fname,end=10)

There are many other options.  Onlyiunitandfnameare required.  If the unit argument is first it does not need the"unit="keyword.

On Unix file names will be _case sensitive_ .

The unit number is assigned by the programmer.

In Unix unit 5 is conventionally standard input and unit 6 is standard output.  Standard error is not as uniform but it is usually unit 2.

Programmers can reassign units 2, 5, and 6, but it is strongly advised that you not do so.

# Popular Options to Open

iostat=ios

Returns status into the integer variableios.  If zero the statement succeeded, otherwise it failed. Specific nonzero values are system-dependent.

err=label

Jumps to statement labeledlabelif an error occurs.

end=label

Jumps to statement labeledlabelon end of file

status=stat

statcan be'old', 'new' , 'replace', 'scratch', or  'unknown'.  The default is 'unknown' (read/write).  If 'old' it must exist, and if'new'it must not exist.  A 'scratch' file is automatically deleted after being closed.

form=fmt

fmtis'formatted'(default, text) or'unformatted'(a system-dependent binary format).

access=acc

acccan be'sequential'(default),'direct', or'stream'. Direct files must be unformatted.

Unless access isstream, an unformatted file will have a header and footer that is specific to a compiler and platform and may not be portable.

position=pos

posis'asis'(default),'rewind', or'append'.  Rewind returns the file pointer to the top.  Append leaves it at the end of the file.

# Inquire

The inquire statement tests the status of a file.  Most usually we wish to check whether the file exists, or is already open, before we attempt to open it.

inquire(unit=iunit,options)

or

inquire(file=fname,optionslist)

So we can inquire by unit or name but not both.

# Common Options to Inquire

* iostat=ios
  * Likeopen
* err=label
  * Likeopen
* exist=exists
* Returns.true.or.false.into logicalexists
* opened=is_open
* Returns.true.or.false.into logicalis_open

# Close

Much of the time, it is not necessary to close a file explicitly.  Files are automatically closed when execution terminates.

If many files are opened, it is good practice to close them before the end of the run.

close(unit=iunit,iostat=ios,err=ier,&

status=st)

Status can be ‘keep’ (default) or ‘delete’

close(iunit)

# REWIND

An open unit can be rewound.  This places the _file pointer_ back to the beginning of the file.

The default is to rewind a file automatically when it is closed.

If you want to rewind the file to reread it, use

rewind(iunit)

Rewind is convenient if the program must handle files whose lengths may vary.  Read through the file without storing any variables, count the number of lines, rewind, then allocate any arrays needed.

If your input files will always be of known length this isn’t necessary (or efficient), but often file length could vary with different data.

# Example

nlines=0

do

read(iunit,*,end=1)

nlines=nlines+1

end do

1 continue

rewind(iunit)

allocate(obs(nlines))

do n=1,nlines

read(iunit,*)obs(n)

enddo

QUIZ

Why do I incrementnlines _after_ the read?

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
* If you do _not_ want to advance, useadvance='no'

