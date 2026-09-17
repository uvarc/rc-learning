---
title: Reductions
date: 2026-08-07:26:29Z
type: book 
weight: 1170
menu: 
    parallel_programming:
        parent: Multithreaded Programming
---

Our race condition resulted from the need to sum a variable across subsums computed by multiple threads.  A similar problem would have occurred for other operations such as multiplication.  This pattern is called a  _reduction_ .  Reductions are so common that OpenMP provides support for them.

We have seen reductions in MPI as a collective communication across processes; OpenMP reductions are similar in that they coordinate results across threads.

A reduction is specified by adding the `reduction` clause to the `parallel for` pragma.  It also requires the reduction operation and reduction variable.

OpenMP takes care of storing partial results in private variables and combining partial results back into the shared varaible after the loop.

The reduction clause has this syntax: `reduction (<op>:<variable>)`

**C/C++ Operators**

{{< table >}}
|  Operator   |  Operation |
|-------------|------------|
|  +          |  Sum       |
|  *          |  Product   |
|  &          |  Bitwise and |
|  |          |  Bitwise or  |
|  ^          |  Bitwise exclusive or  |
|  &&         |  Logical and |
|  ||         |  Logical or |
|  max        |  maximum value |
|  min        |  minimum value |
{{< /table >}}

OpenMP 3.1 or later is required for support of max and min in C/C++, but all recent compilers should implement at least this version.

Syntax:

```c
double area, pi, x;
int i, n;
...
area = 0.0;
#pragma omp parallel for private(x) reduction(+:area)
for (int i=0; i<n; i++) {
     x = (i + 0.5)/n;
     area += 4.0/(1.0 + x*x);
}
pi = area / n;
```
Similarly for Fortran
```fortran
!$omp parallel for private(x) reduction(+:area)
```

**Fortran Operators**

{{< table >}}
|  Operator   |  Operation |
|-------------|------------|
|  +          |  Sum       |
|  *          |  Product   |
|  .iand.     |  Bitwise and |
|  .ior       |  Bitwise or  |
|  .ieor.     |  Bitwise exclusive or  |
|  .and.      |  Logical and |
|  .or.       |  Logical or |
|  .eqv.      |  Logical equivalence |
|  .neqv.     |  Logical nonequivalence |
|  max        |  maximum value |
|  min        |  minimum value |
{{< /table >}}


**Exercise**

Modify the pi-computing code to use an appropriate reduction.

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/omp_reduction_area.c" lang="c" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/omp_reduction_area.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/omp_reduction_area.py" lang="py" >}}
{{< /spoiler >}}

