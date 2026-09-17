---
title: Critical Sections and Atomic Operations
date: 2026-08-03:26:29Z
type: book 
weight: 1160
menu: 
    parallel_programming:
        parent: Multithreaded Programming
---

Critical sections are blocking; no thread can enter a critical section until the thread executing the block has exited.  It is illegal to jump into or out of a critical section; the code block must be executed in its entirely by all threads.

It is possible to use _named_ critical sections to partially minimize the serialization, but this is beyond our scope.

Critical sections can surround any block of code of any length. However, a frequent pattern is the need for only one operation to be performed one thread at a time. 

In the special circumstances of the pi-computing code, we can use an _atomic operation_ rather than a critical section. Atomic operations are limited to "storage" access, which in this context means memory.  The operation can `read` the storage location, `write` to it, or `update` it.  These specifics can be added as clauses to the construct, but are not usually necessary and were added in recent versions of the standard.

Without a clause (or when the clause is `update`), the following operations are supported:

```
x++;  // C/C++
x--;  // C/C++
++x;  // C/C++
--x;  // C/C++
x binop= expr
x = x binop expr
x = expr binop x
```
where `binop` is one of one of `+`, `*`, `-`, `/`, `&`, `^`, `|`, `<<`, or  `>>`. The operation must be supported by the underlying language. The `expr` is an arithmetic expression with the permitted operations.

The `atomic` directive applies only to the statement following it.

Atomic operations can have lower overhead than using a critical section for the same statement, and if the architecture supports it they can use hardware atomic operations to further reduce overhead. Also, if two independent statements are critical, they can each be atomic and different threads can execute them at the same time, as long as only one thread performs the operation.

Atomic operations still serialize the code, however. 


**Syntax**

C/C++
```c
#pragma omp atomic <clause>
```

Fortran
```fortran
!$omp atomic <clause>
! optional
!$omp end atomic
```
The end is optional since atomic is only valid for the following statement.

Python
Omp4py does not seem to have fully implemented this yet.

**Exercise** 

Modify the pi code to use an atomic construct.

**Solutions**

{{< spoiler text="C" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/omp_atomic.c" lang="c" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/omp_atomic.f90" lang="fortran" >}}
{{< /spoiler >}}

