---
title: Parallel For
date: 2026-08-03:26:29Z
type: book 
weight: 1100
menu: 
    parallel_programming:
        parent: Multithreaded Programming
---

Data parallelism is frequently expressed as loops.  OpenMP provides a `parallel for` construct (`parallel do` for Fortran).  Threads are forked at the directive.  The region persists through the immediately following code block. The compiler must be able to verify the run-time system will have all the information it needs to schedule loop iterations among the threads, but this is usually the case.

**C++**
```c++
#pragma omp parallel for
for (i = 0; i < N; i++) {
   a[i] = b[i] + c[i];
}
```

**Fortran**
Fortran marks both beginning and end of the parallel region.
```fortran
!$omp parallel do
   do i=1, N
      a(i) = b(i) + c(i)
   enddo
!$omp end parallel do
```

OpenMP provides a special directive for Fortran array operations, the `workshare`.
```fortran
!$omp PARALLEL WORKSHARE
A=1.
B=42.
C=2.*B
A=B*C+D
!$omp END PARALLEL WORKSHARE
```

Operations permitted with a `workshare` are array and scalar assignments, FORALL constructs and statements, WHERE constructs and statements, atomic and critical operations, and parallel constructs.  Function calls are allowed but only for ELEMENTAL functions.  Not all statements permitted will parallelize (FORALL and WHERE do not, nor do scalar assignments, for instance). See the OpenMP documentation for more details.

**Python**
```
@omp
def loop(N):
    with omp("parallel for"):
        a[i]=b[i]+c[i]
```

## num_threads

After the "parallel for" or `parallel do` in the directive, we can include a _clause_ `num_threads(nthreads)` which forces the parallel for to run on the specified number of threads.  This is sometimes useful even when we have a larger number available, but in our examples it was used for consistency.

The clause is equivalent to the `omp_set_threads(n)` function. The function can be invoked anywhere before a parallel region, whereas the clause is only recognized by the `parallel`, `for`/`do` (and `parallel for`) and `section` directives.

The `num_threads` clause takes precedence over the `set_num_threads` function, and either will override the environment variable `OMP_NUM_THREADS`.

## Limitations

The `parallel for` or `parallel do` directives require that the loop be a standard for/do loop with no form of exit possible. The compiler may even be unable or unwilling to parallelize it if the increment has any value other than `1`.

At the end of the parallel for region is an implied **barrier**. As threads finish, they will stop and wait for the other threads to complete before the parallel region is exited.  This keeps the threads synchronized but also serializes the code.

### nowait clause

If you are sure that synchronization is not required, you can add the `nowait` clause.

```c
#pragma omp parallel for nowait
```
```fortran
!$omp parallel for nowait
```
```python
with omp("parallel for nowait")
```

## Example

These simple codes illustrate how a loop is distributed among threads.

Compile and run (C or Fortran) or run in the appropriate environment (Python). 

If you like, and have a larger number of cores available, change the upper loop bound and the number of threads.  Try a number of iterations that is not evenly divisible by the number of threads.

## C++

{{< code-download file="/courses/parallel-computing-introduction/codes/omp_parfor.c" lang="c" >}}


## Fortran

{{< code-download file="/courses/parallel-computing-introduction/codes/omp_parfor.f90" lang="fortran" >}}


## Python

{{< code-download file="/courses/parallel-computing-introduction/codes/omp_parfor.py" lang="python" >}}


