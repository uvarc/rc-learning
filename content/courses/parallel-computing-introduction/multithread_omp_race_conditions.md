---
title: Race Conditions and Criticality
date: 2026-08-03:26:29Z
type: book 
weight: 1150
menu: 
    parallel_programming:
        parent: Multithreaded Programming
---

Consider this C program fragment to compute $pi$ using the rectangle rule:

```c
double area, pi, x;
int i, n;
...
area = 0.0;
for (i = 0; i < n; i++) {
    x = (i+0.5/n);
    area += 4.0/(1.0 + x*x);
}
pi = area/n;
```

If we naively parallelize the loop
```c
double area, pi, x;
int i, n;
...
area = 0.0;
#pragma omp parallel for private(x)
for (int i=0; i<n; i++) {
    x = (i+0.5/n);
    area += 4.0/(1.0 + x*x);
}
pi = area/n;
```
we set up a **race condition** in which one process may “race ahead” of another and not see its change to shared variable `area`.

:{{< figure src="/courses/parallel-computing-introduction/img/race_condition.png" alt="Race condition due to uncontrolled access to main memory by multiple threads." caption="Two threads compute area without coordination. The correct result should be 18.995"  >}}

We can illustrate what is happening by considering a timeline. We will limit ourselves to two threads for the purpose of illustration. Both read a value from main memory, then increment according to where they are in their individual loops.  Thread 0 is faster than Thread 1 and writes its result to main memory. Thread 1 finishes its update and writes its value of `area` back to main memory without using the value updated by Thread 0. 

We cannot make `area` a private variable, because at most one value of a private variable will be written back to memory (with `lastprivate` when the parallel region terminates.  Private variables are all temporary and do not persist beyond the parallel region. The race condition means that we are very unlikely to obtain a correct result.

{{< figure src="/courses/parallel-computing-introduction/img/race_timeline.png" alt="Race condition timeline." caption="Race Condition Timeline."  >}}

## Critical Sections

A critical section is a portion of a parallel region that only  _one_  thread at a time may execute.  We denote a critical section with the `critical` directive.

```c
#pragma omp critical
```
in front of a block of C.

For Fortran the equivalent is
```fortran
$omp critical
$end omp critical
```

### Correcting the Pi Calculation

We will enclose the update to area inside a critical section
```c++
double area, pi, x;
int i, n;
...
area = 0.0;
#pragma omp parallel for private(x)
for (int i = 0; i < n; i++) {
    x = (i+0.5)/n;
#pragma omp critical
    area += 4.0/(1.0 + x*x);
}
pi = area / n;
```

This solves the problem and returns the correct result, but at the cost of efficiency.  Only one thread at a time may execute the statement; i.e., it is sequential code.  And the time to execute this statement is a significant part of loop.Consequently, our parallel speedup will be severely constrained by this serialization.

Critical sections cause serialization, but if used judiciously they can avoid race conditions without seriously impacting efficiency. 

**Exercise** 

Download the full code for your language. Compile (for C/C++/Fortran) and run with the race condition.  Uncomment the critical directive and run it again.

{{< spoiler text="C" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_critical_area.c" lang="c" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_critical_area.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_critical_area.py" lang="py" >}}
{{< /spoiler >}}

Run the codes for your language of choice.  For C and Fortran, also compile without OpenMP to run as a serial code and compare the results.
