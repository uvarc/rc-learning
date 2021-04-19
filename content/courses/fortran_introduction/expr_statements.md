---
title: "Expressions and Statements"
toc: true
type: docs
weight: 30

menu:
    fortran_introduction:
        name: Expressions and Statements
        weight: 30

---

# Expressions 

An _expression_ is a combination of 

Fortran expressions are much like those of other languages.
```fortran
a+3*c
w=8.d0*real(i,dp)+v**3
z=phase+cmplx(0.,1.)
sqrt(abs(a-b))
A .or. B
y > 0.0 .and. y < 1.0
z=myfunc(x,y)
```

# Statements

A Fortran peculiarity: statements are _executable_ or _non-executable_ .  Non-executable statements are instructions to the compiler (variable declarations, interfaces, etc.)  Executable statements perform some action. All non-executable statements must _precede_ the first executable statements in a program unit.

Indentation is not required but _should be_ used!

No semicolons should be used at the end of the line.

Multiple statements (keep them short) may be written on the same line if they are separated by semicolons.

# Fixed Format versus Free Format

* Prior to the Fortran 90 standard, Fortran code was required to conform to rigid column rules based on the layout of punched cards.
  * This may be another reason that computer scientists sneer at it
  * Statements began in column 7 and could extend to column 72.  Column 6 was reserved for continuation marks.  Columns 1-5 were for statement labels.  Columns 73-80 were ignored (and were used to number cards)
* In Fortran 90 and up, there are no column rules.  This is called free format.

<img src="img/Introduction to Fortran7.jpg" width=500px />

# Comments, Continuations, Etc.

* Fixed format comment:
  * Corcin the first column meant the entire line was a comment.
* Free format comment:
  * Anything from!to the end of the line is ignored.
* Fixed format continuation:
  * Number or printable character in the 6thcolumn.
* Free format continuation:
  * Ampersand&at the end of the line to be continued.

# Statements for the Main Program

A program may optionally begin with aPROGRAMstatement which is optionally followed by its name.

PROGRAMmyprogram

The program must end with anENDstatement.  Optionally it may be

END PROGRAM <name>

I strongly recommend use of the longer, more descriptive forms.

Execution may be stopped with theSTOPstatement.STOPis required only for abnormal termination.  It can optionally be followed by a message, which it will print to standard output.

STOP "Attempt to divide by zero."

# IMPLICIT

For historical reasons, Fortran can use implicit typing.  By default, variable names beginning inA-HandO-ZareREAL(single precision) while variable names beginning inI-N(the first two letters ofINteger) are integers.

IMPLICITstatements change this behavior.  Older code often changes the default float to double

IMPLICIT DOUBLE PRECISION(a-h,o-z)

New code should always cancel implicit typing with

IMPLICIT NONE

This requires that all variables be declared and will catch

many “typo” bugs.

TheIMPLICITstatement must appear in each program unit.

# Statement Labels

In fixed format code, _statement labels_ were often used.

In fixed format statement labels must be integers and must occupy a maximum of five digits, at least one of which must be nonzero.

In free format there is less need for labels and they do not always need to be integers.

Any statement that is not part of a compound (semicolon-separated) statement can be labeled with an integer.

# Miscellaneous

* The no-op iscontinue
  * It was often used in old code since do loops required a labeled statement as the terminator.
  * do 100i=1,n
  * statements
  * 100 continue
  * We useend donow for this purpose.  However,continueis a convenient target for labels in input/output statements.
* Fortran has ago to (orgoto)statement.
* go to <label>
  * Despite computer science hatred for it, it is still sometimes useful and in some situations it is far more readable than the contortions required to replace it!
  * As a rulegotoshould always direct the flow downward, never upward.

# Hello World

program hello

implicit none

integer  ::i

real     :: pi=0.25*atan(1.0)

i=42

print *, 'Hello! The answer is', &i,' and pi is ',pi

end program

# Exercise

* Write a program that will set variables as indicated and will print the expressions indicated.  Use print * to print to the console. Invoke implicit none and declare all your variables.
  * x=17.
  * Xs=11.
  * num_1=10
  * num_2=14
  * print *, x
  * print *,Xs/x
  * print *,int(Xs/x)
  * print *,int(Xs)/int(x)
  * print *,Xs/x + x
  * print *,Xs/(x+x)
  * print *, x/num_1
  * print *, num_1/num_2
  * print *, num_2/num_1

Declare character variables large enough to hold the indicated strings.  Makenewtitleat least 5 characters longer than you think necessary.

title=“This is a string”

subtitle=“Another string”

print *,len(title)

print *, title//”:”//subtitle

newtitle=title//” : “//subtitle

print *,len(newtitle)

print *,len_trim(newtitle)

print *,newtitle(2:4)

!Change “This” to “That” innewtitle

Exercises with conditionals.

Be sure to declare variables appropriately.

a=11.; b=9.; c=45.; n=3

print *, a>b

print *, a<b and c==n

print *, a<b or c==n

print *, a>b or c==n and a<b

print *, (a>b or c==n) and a<b

is_equal= a==b

print *,is_equal

# KIND

Variables in modern Fortran may have a _kind_ associated with them. The programmer requests at least a certain number of decimal digits of precision and at least a certain exponent range, and the system matches the request as best it can.

In practice, most systems have single and double precision.  A few offer quad precision (REAL*16in older nomenclature) but it is usually done in software and is _very_ slow.

Compilers still supportREALandDOUBLE PRECISION(REAL*8was never standard, but is fairly universally supported).  The advantage toKINDis that it becomes easy to change precision, especially when using a module.

# Obtaining KIND Information

NEVER assume that theKINDtype integer equals the number of bytes.  Useintrinsicsto obtain or useKIND. The range is10range

ik=selected_int_kind(range)

rk=selected_real_kind(prec,range)

kind(x)returns the kind type parameter ofx

ik=kind(1)

rk=kind(1.0)

dk=kind(1.0d0)

# Declaring KIND

For the IEEE 754 standard, the two kinds supported in hardware can be selected with:

INTEGER, PARAMETER ::rk=kind(1.0)

INTEGER, PARAMETER ::rk=kind(1.d0)

The kind variable needs to be aparameter.

Variables are then declared as

REAL(rk)  :: r, s, t

Literals can be written as

1.0_rk

Switching between single and double precision is then as easy as replacing theSELECTED_REAL_KINDorKINDstatement.  It is best to use a module for this purpose.

# Type Conversions with KIND

* Thekindargument is optional except forrealconverting a floating-point type to another FP type.
* aint(a,kind)  !truncates a float
* anint(a,kind) !nearest integer
* ceiling(a,kind)
* cmplx(x,y,kind)
* floor(a,kind)
* int(a,kind)  !truncates, casts to integer
* nint(a,kind) !nearest integer, casts to integer
* real(a,kind)!converts integer to float or & 				between float types
* Example: explicit casting with kind
  * Given thatdphas been declared to match double:
  * x=real(w,dp)
  * convertswfrom single to double precision.

