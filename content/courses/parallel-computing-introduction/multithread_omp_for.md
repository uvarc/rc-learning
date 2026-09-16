---
title: The For Directive
date: 2026-08-12T17:26:29Z
type: book 
weight: 1210
menu: 
    parallel-programming:
        parent: Multithreaded Programming
---

Within a `parallel` region we often want to implement a for loop. If we use `for` alone, the entire loop will be replicated by each thread, when we want it distributed among the team.  But using `parallel for` will not usually work or do what we want. OpenMP provides a directive specifically for this situation, the `fo` directive. It should be used within a `parallel` region.

Like the `omp parallel for`, this construct requires that the for/do loop be in _canonical form_, as defined by the OpenMP [standard](https://www.openmp.org/spec-html/5.2/openmpsu28.html#x58-600004.4.1). For the most part, this means the loop variable must be an integer, the upper bound must be a simple arithmetic expression involving the loop variable, and the increment must be an addition and/or multiplication of the loop variable.  (See the standard section cited above for detailed specifics.)

Syntax:

#### C/C++

```c
#pragma omp parallel
{
    code
    #pragma omp for
    for (int i=0; i<N; i++) {
        code
    }
}
```

**Example**

{{< spoiler text="C" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_parallel.c" lang="c++" >}}
{{< /spoiler >}}


#### Fortran

```fortran
!$omp parallel
    code
    !$omp do
        code
    !$omp end do
!$omp end parallel
```
Reminder to Fortran programmers: Fortran parallel regions often require a `private` clause because most Fortran programs do not use block statements and variables are not declared within the parallel region.


**Example**

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_parallel.f90" lang="fortran" >}}
{{< /spoiler >}}


#### Python (for omp4py)

```python
with omp("parallel"):
    code
    with omp("for"):
        code
```

Like Fortran, Python can also require a `private` clause. In this example, the omp for directive can only include a for loop, nothing more, so we initialize its loop variable outside it and thus must add the private clause.

**Example**

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_parallel.py" lang="python" >}}
{{< /spoiler >}}

