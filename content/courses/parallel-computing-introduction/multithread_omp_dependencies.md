---
title: Dependencies
date: 2026-08-03:26:29Z
type: book 
weight: 1130
menu: 
    parallel_programming:
        parent: Multithreaded Programming
---

By using a parallel for, you are telling the compiler that there are no inter-iteration loop dependencies; that is, the loop iterations are completely independent of one another. If this is not true, multiple problems (and incorrect results) can occur.

The loop cannot contain statements that terminate the loop prematurely. No `break`, `return`, `exit`, or  `goto` (C/C++, Python) or `exit`, `return`, `stop`, or `go to` (Fortran).   

Iterations can be skipped; `continue` (C/C++, Python) or `cycle` (Fortran) is permitted.

## Types of Data Dependency

Computer scientists categorize data dependencies into three types.

### True Dependence

A loop contains a true dependency if a variable's value is written to memory before it is read. A simple example is

```c++
a[0]=0;
for (i=1; i<N; i++) {
    a[i]=a[i-1]+10.;
}
```
The value at index `i` depends on the value set at the previous iteration. In a serial code, this does not cause a problem because a[i-1] has been computed and written. In a threaded code, a[i] and a[i-1] are not necessary accessed by the same thread, so the updated a[i-1] may not have been written when another thread wants to read it.  This is also called a _read after write_ dependence.

{{< spoiler text="C" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_truedep.c" lang="c" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_truedep.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_truedep.py" lang="py" >}}
{{< /spoiler >}}

### Anti-Dependence

This is a _write after_read_ dependence.

```c++
for (i=0; i<N-1; i++) {
    a[i]=a[i+1]+10.;
}
```

In a serial code, the intention in such a loop would normally be to use the old value of a[i+1] to update the current a[i], but this cannot be guaranteed in a threading environment.

{{< spoiler text="C" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_antidep.c" lang="c" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_antidep.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_antidep.py" lang="py" >}}
{{< /spoiler >}}

### Output Dependence

This is a _write_after_write_ dependence. A location in memory must be written before it is written again. 

```c++
for (i=0; i<N-1; i++) {
    a[i] = i;
    a[i+1] = x+i;
}
```

This pattern might be fairly unusual even in serial code, but it can happen. At iteration `i` the value of a[i+1] was located in memory at a[i]. If it is accessed before a[i], an incorrect result can be computed.

When run in serial, a loop such as this will overwrite all but the last value of `a`.  Run threaded in C or Fortran, it usually gets the expected result but some values are incorrect. 

{{< spoiler text="C" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_outdep.c" lang="c" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_outdep.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/omp_outdep.py" lang="py" >}}
{{< /spoiler >}}

**Exercise**

Run the codes for your language of choice.  For C and Fortran, also compile without OpenMP to run as a serial code and compare the results.
