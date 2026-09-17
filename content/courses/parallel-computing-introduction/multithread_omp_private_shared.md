---
title: Private and Shared Variables
date: 2026-08-03:26:29Z
type: book 
weight: 1120
menu: 
    parallel_programming:
        parent: Multithreaded Programming
---

In threaded code, a variable can be _shared_ or _private_ within a parallel region.

A shared variable has the _same_ address in the execution context of every thread.

A private variable has a  _different_  address in the execution context of every thread.

A thread cannot access the private variables of another thread. All threads can access shared variables.

There are default criteria for which variables are shared and which are private that are to some extent language-dependent.

The default for C/C++ (and Python where relevant) is all variables declared `static`, and all in whole-file scope are **shared**.  Within a parallel region, anything not private is also shared.

In Fortran, by default all variables in `COMMON` or declared `SAVE` are **shared**. All module (before the CONTAINS) are shared.

The default for **private** variables is the first loop variable encountered (all languages). Variables on the _stack_ in functions (C/C++) or subroutines (Fortran) that are invoked within a parallel region are also private by default.

_Stack_ is a segment of memory used to store temporary variables in subprograms.Stack variables will go out of scope or may be automatically deallocated when the subprogram exits.  

In C/C++, variables declared within a parallel region are private by default. In Fortran variables must be declared within the nonexecutable preamble to executable code, unless a BLOCK/END BLOCK section is declared.  Variables can be declared within the block and these are private by default.

The defaults can be overridden with the `private`, `firstprivate`, `lastprivate`, `shared`, `default`, `reduction`, and `copyin` clauses to the directive. The clauses can be applied to a limited set of directives

## Private Clauses

### Private

A typical multidimensional loop is

```c++
for (i = 0; i < N; i++) {
    for (j = 0; j <N; j++) {
        a[i][j] = myMIN(a[i][j])+myMEAN(a[i][j];
    }
}
```

Either loop could be executed in parallel, but we prefer to make outer loops parallel, to reduce the number of forks/joins.

We then must give each thread its own private copy of variable j. The _private_ clause directs the compiler to make one or more variables private.  The syntax is the same for all languages.

```
private(<variable list>)
```

The private variable is set up within each thread and has no specific value until initialized within the parallel region.

**C/C++**

```c++
#pragma omp parallel for private(j)
    for (i = 0; i <N; i++)
        for (j = 0; j < N; j++)
            a[i][j] = myMIN(a[i][j)+myMEAN(a[i][j]);
```

Note that according to our rule for defaults, we could also make both loop indices as follows, without the need for the clause:
```c++
#pragma omp parallel for 
    for (int i = 0; i <N; i++)
        for (int j = 0; j < N; j++)
            a[i][j] = myMIN(a[i][j)+myMEAN(a[i][j]);
```

**Fortran**

Note the loop order for cache efficiency.
```fortran
!$omp parallel do private(i)
do j=1,N
    do i=1,N
        a(i,j)=min(a(i,j),a(i,j)+tmp)
    enddo
enddo
!$omp end parallel do
```

**Python**
Wrapping function omitted.

```python
with omp("parallel for private(j)"):
    for i in range(N):
        for j in range(N):
            a[i,j]=min(a[i,j],a[i,j]+tmp)
```

#### Firstprivate/Lastprivate

The `firstprivate` clause is like `private but additionally initializes the private variable to its last value outside the parallel region.  

The `lastprivate`clause sets the corresponding variable outside the parallel region to the final private value from the thread that executed the last iteration (in the case of for/do).

**Example**

These examples illustrate private, firstprivate, and lastprivate. Note that the Fortran version uses both the `firstprivate` and `private` clauses; this is permitted and fairly common, especially in Fortran.

The lastprivate clause is not yet implemented in OMP4Py at this time.

## C++

{{< code-download file="/courses/parallel-computing-introduction/codes/omp_private_firstlast.c" lang="c" >}}

## Fortran

{{< code-download file="/courses/parallel-computing-introduction/codes/omp_private_firstlast.f90" lang="fortran" >}}

## Python

{{< code-download file="/courses/parallel-computing-introduction/codes/omp_private_firstlast.py" lang="python" >}}


