---
date : "2021-04-5T00:00:00-05:00"
title : "Starting with Fortran"
toc: true
type: book
weight: 1

menu:
    fortran_introduction:
        parent: Starting with Fortran
        weight: 2

---

## Some History

### Fortran and Algol

Algol 60 is of similar age and similar design.
Some sample code (from Wikipedia):
```plaintext
procedure Absmax(a) Size:(n, m) Result:(y) Subscripts:(i, k);
   value n, m; array a; integer n, m,i, k; real y;
comment The absolute greatest element of the matrix a, of size n by m, is transferred to y, and the subscripts of this element to i and k;
begin integer p, q;
    y := 0; i := k:= 1;
    for p := 1 step 1 until n do
       for q := 1 step 1 until m do
           if abs(a[p, q]) > y then  
           begin
              y := abs(a[p, q]); i:= p; k := q
           end
end Absmax
```
Note that in Algol, a `begin`/`end` block is logically a single statement.

Similar code in early Fortran would look like
```fortran
      SUBROUTINE ABSMAX(A,N,M,I,K,Y)
      INTEGER N, M
      INTEGER A
      DIMENSION A(N,M)
      INTEGER Y
      INTEGER I, K
      INTEGER P, Q
C  THE ABSOLUTE GREATEST ELEMENT OF ARRAY A OF SIZE NxM IS COMPUTED AND RETURNED
C  IN Y. THE CORRESPONDING LOCATION SUBSCRIPTS ARE RETURNED IN I AND K.
      Y=0
      I=1
      K=1
      DO 200 P=1,N
          DO 100 Q=1,M
             IF ( ABS(A(P,Q)) .LE. Y) GO TO 10
                Y=ABS(A(P,Q))
                I=P
                K=Q
   10        CONTINUE  
  100     CONTINUE
  200 CONTINUE
      END
```
Some of the peculiarities of the first versions of Fortran, such as FORTRAN IV, were due to IBM's use of _punch cards_ as input devices.

{{< figure src="/courses/fortran_introduction/img/PunchCard.jpg" width=500px caption="80-column punch card" >}}

Punch cards were prepared on machines called _keypunches_. The character set on a keypunch was limited, so Fortran used all capital letters and few other characters.  There was no opportunity to correct typograpical errors, which may account for early Fortran ignoring spacing within keywords. The layout in Fortran was determined by the physical layout of the cards; this, combined with the need to write a simple, memory-conserving compiler, resulted in a strict column-oriented syntax.  
This is called _fixed format_.  It required that the first column be reserved for the `C` that introduced a comment.  The next four columns were for numerical statement labels.  Column six was reserved for _continuation_ characters.  Statements began in column 7 and could extend through column 72; anything from column 72 to 80 was ignored by the compiler.  These columns were frequently used to number the cards.

Algol, on the other hand, was written for _paper tape_.  Whereas punch cards are _record_ (i.e. line) _oriented_, paper tape is a continuous medium, so the semicolon marked the end of a statement.  Algol's descendants, which include C and C++, were also written for paper tape.  Semicolons continue to be used in many languages to mark the end of a sentence; such languages do not use continuation markers.  Record-oriented languages such as Fortran and Python do not require statement markers (though both modern Fortran and Python allow them) and provide for continuations.

### Newer Fortran

The language has changed dramatically since 1957 but the name has never changed.
The above subroutine in modern, _free format_ Fortran is
```fortran
subroutine absmax(a,i,k,y)
!  The absolute greatest element of array A of size NxM is computed and returned
!  in Y. The corresponding location subscripts are returned in I and K.
   integer, dimension(:,:), intent(in) :: a
   integer,                 intent(out):: i,k
   integer,                 intent(out):: y
   integer                             :: p,q

   y=0; i=1; k=1
   do p=1,size(a,1)
      do q=1,size(a,2)
         if (abs(a(p,q)) > y) then
            y=abs(a(p,q))
            i=p
            k=q
         endif
      enddo
   enddo
end subroutine
```

## Strengths and Weaknesses

|Fortran        |  C++ (not C)   |
|---------------|----------------|
|(2003/8) Many math function built-ins |Limited mathematical built-ins |
|Multidimensional arrays a first-class data structure, array operations supported| True multidimensional arrays not possible without add-on libraries (Blitz++, Boost)|
|Does not support true strings yet, just character arrays| Good string handling (compared to C) |
|Classes somewhat clunky.  Modules fill much of this role| Straightforward implementation of classes (modules in C++20 standard) |

## Compiled Languages

Fortran and C++ are _compiled_ languages.  Readers who are accustomed to 
interpreted languages such as Python, R, and MATLAB should be aware that
compiled languages are generally more complex than interpreted languages and 
require more steps in the development process. 

A __compiler__ produces a stand-alone program for a given _platform_ (cpu+operatingsystem).  The output of a compiler is an _object file_, represented with a.osuffix on Unix.  Object files are in machine language but cannot be run independently.

A __linker__ takes the .o files and any external _libraries_ and links them into the _executable_.  Normally the linker is invoked through the compiler.

An __interpreter__ interprets line by line.  The executable that is run is the interpreter itself.  Programs for interpreters are often called _scripts_.  Scripts are frequently cross platform, but the interpreter itself must be appropriate to the platform.

Compared to interpreted languages such as Python, compiled languages are:
  * Generally stricter about typing (static typing) and memory allocation.
  * Memory must frequently be managed explicitly by the programmer.
  * Generally produce faster and more efficient runs.
Interpreted languages are:
  * Generally looser about typing (dynamic typing).
  * Generally have dynamically sized data structures built in.
  * Often run very slowly.

The workflow for compiled code consists of compiling, correcting syntax errors if necessary, then linking.  The process of compiling and linking is generally called _building_.  The product of a build cycle is the _executable_ (also called a _binary_).  For each change, the entire process must be repeated.  It is easy to forget to recompile, then wonder why the change is not reflected in the output.
