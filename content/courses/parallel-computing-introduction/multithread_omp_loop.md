---
title: The Loop Directive
date: 2026-08-19T17:26:29Z
type: book 
weight: 1230
menu: 
    parallel-programming:
        parent: Multithreaded Programming
---

The `omp for` construct is very restrictive. The OpenMP standard introduced the more flexible `omp loop` construct to overcome such limitations and most especially to make it easier to instantiate threads on devices such as GPUs.

We have not discussed thread scheduling, but this can be a significant issue with devices, and implementations can choose scheduling (if not specified by the programmer) to be more efficient on devices.  The `loop` directive can also be used with newer parallel region constructs such as `teams` as well as `parallel`.

Another significant difference between `for` and `loop` is that the former contains an implicit barrier at termination, so that all threads will synchronize, whereas the latter does not and on much be added explicitly if it is needed.

The loop construct still requires canonical form for for/do loops (contrary to some online sources), but newer OpenMP standards have considerably expanded what is accepted as "canonical."

The directive is the same for both C/C++ and Fortran.

Syntax:

#### C/C++

```c
#pragma omp parallel
{
    code
    #pragma omp loop
    for (int i=0; i<N; i++) {
        code
    }
}
```

**Example**

{{< spoiler text="C" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_parallel.c" lang="c" >}}
{{< /spoiler >}}

#### Fortran

```fortran
!$omp parallel
    code
    !$omp loop
       code
    !$omp end parallel
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

```

Like Fortran, Python can also require a `private` clause. In this example, the omp for directive can only include a for loop, nothing more, so we initialize its loop variable outside it and thus must add the private clause.

**Example**

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_parallel.py" lang="python" >}}
{{< /spoiler >}}
