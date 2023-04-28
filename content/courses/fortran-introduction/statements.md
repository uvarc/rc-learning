---
title: "Statements"
toc: true
type: book
weight: 25

menu:
    fortran-introduction:
        parent: Statementns
        weight: 25

---

If expressions are the "words," then statements are the "sentences" of a programming language.  A statement is a combination of expressions and operators such as assignment (`=` in most languages) which describes a command to the compiler.

A Fortran peculiarity: statements are _executable_ or _non-executable_ .  Non-executable statements are instructions to the compiler (variable declarations, interfaces, etc.)  Executable statements perform some action. All non-executable statements must _precede_ the first executable statements in a program unit.

Indentation is not required but _should be_ used!
No semicolons should be used at the end of the line.
Multiple statements may be written on the same line if they are separated by semicolons.  If this is done each statement should be kept short.

## Fixed Format versus Free Format

Prior to the Fortran 90 standard, Fortran code was required to conform to rigid column rules based on the layout of punched cards.
Statements began in column 7 and could extend to column 72.  Column 6 was reserved for continuation marks.  Columns 1-5 were for statement labels.  Columns 73-80 were ignored (and were used to number cards).  Because of the placement restrictions, this is called **fixed format**.
In Fortran 90 and up, there are no column rules.  This is called **free format**.

## Comments and Continuations

_Comments_ are ignored by the compiler.  They are for the benefit of human readers and writers.

Fixed format comment:
  * `C` or `c` in the first column meant the entire line was a comment.

Free format comment:
  * Anything from `!` to the end of the line is ignored.

Due to its record-oriented history, Fortran uses the end-of-line marker to terminate a statement.  If the statement is too long to fit, or for esthetic reasons the programmer wishes to extend the statement over multiple lines, a continuation marker must be used.

Fixed format continuation:
  * A number or printable character in the 6th column.

Free format continuation:
  * Ampersand `&` at the end of the line to be continued.

The maximum number of statement characters in a line for fixed format can be modified through a compiler option, with the default the traditional 72.  The default maximum line width for free format is 132 columns, but it can also be modified through compiler options in most cases.  For the case of gfortran, see their [documentation](https://gcc.gnu.org/onlinedocs/gcc-4.2.1/gfortran/Fortran-Dialect-Options.html).  For other compilers, consult their documentation.

## Program Statement

A program may optionally begin with a PROGRAM statement which is optionally followed by its name.
```
PROGRAM myprogram
```
The program must end with an `END` statement.  Optionally it may be
```
END PROGRAM <name>
```
Use of the longer, more descriptive forms is strongly recommended.

Execution may be terminated with the `STOP` statement. `STOP` is required only for abnormal termination.  It can optionally be followed by a message, which it will print to the console.
```
STOP "Attempt to divide by zero."
```

## Statement Labels

In fixed format code, _statement labels_ were often used.
In fixed format statement labels must be integers and must occupy a maximum of five digits, at least one of which must be nonzero.
In free format there is less need for labels and they do not always need to be integers.
Any statement that is not part of a compound (semicolon-separated) statement can be labeled with an integer.

## Miscellaneous

The no-op (do nothing) is `CONTINUE`.
It was often used in old code since do loops required a labeled statement as the terminator.
```fortran
      do 100 i=1,n
         statements
100   continue
```
We use `END DO` now for this purpose.  However, `CONTINUE` is a convenient 
target for labels in input/output statements, and occasionally in other circumstances.

Fortran has a `GO TO (or `GOTO`) statement.
```fortran
    go to <label>
```
Modern practice is to avoid `go to` and some languages, such as Python, do not even provide it.  However, it is still sometimes useful and in some situations 
it is far more readable than the contortions required to replace it!
* As a rule `goto` should always direct the flow downward, never upward.

## Hello World
An improved example:
```fortran
program hello
implicit none
   integer  :: i
   real     :: pi=0.25*atan(1.0)
      i=42
      print *, 'Hello! The answer is', &i,' and pi is ',pi
end program
```

**Exercise**
* Write a program that will set variables as indicated and will print the expressions indicated.  Use `print *` as shown above to print to the console. Invoke 
`implicit none` and declare all your variables.
```fortran
x=17.
Xs=11.
num_1=10
num_2=14
! Print the following expressions (remove comment marker)
!x
!Xs/x
!int(Xs/x)
!int(Xs)/int(x)
!Xs/x + x
!Xs/(x+x)
!x/num_1
!num_1/num_2
!num_2/num_1
```

{{< spoiler text="Example Solution" >}}
{{< code file="courses/fortran-introduction/solns/statements.f90" lang="fortran" >}}
{{< /spoiler >}}

