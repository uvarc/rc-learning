---
title: OpenACC Kernels and Parallel Regions
date: 2026-09-2T17:26:29Z
type: book 
weight: 4070
menu: 
    parallel_programming:
        parent: Accelerator Programming   
---

## Kernels

A _kernel_ for OpenACC is a region of code that could be executed on the device.  With the keyword `kernels` the compiler determines what can be offloaded to the device, and sets up the system to transfer the instructions and data appropriately..


```c
#pragma acc kernels
```

```fortran
!$acc kernels
!$acc end kernels
```

**Offloading loops**

In general, a compiler cannot predict whether two arrays refer to the same pointer (aliasing), in which case OpenACC will not parallelize the loop. C/C++ programmers should use keywords like `const` and `restrict` to limit aliasing.

```c
#pragma acc kernels
{
    for (int i=0; i<n; ++i) {
        a[i] = 0.0;
        b[i] = 1.0;
        c[i] = 2.0;
    }

    for (int i=0; i<n; ++i) {
        a[i]=b[i]+c[i];
    }
}

```fortran
!$acc kernels

do i=1,n
    a(i) = 0.0
    b(i) = 1.0
    c(i) = 2.0
end do

do i=1,n
    a(i) = b(i)+c(i)
enddo

!$acc end kernels
```

## Parallel and Parallel Loop

Like OpenMP, OpenACC has a `parallel` directive.  However, it is usualll used with the `loop` directive. 

The major difference between `kernels` and `parallel loop` is that with `parallel`, similarly to the corresponding directive in OpenMP, the programmer is asserting that the loop can be safely parallelized and offloaded to the device. This is usually, though not always, more efficient than autoparallelization from the compiler. 

Note that unlike OpenMP, the OpenACC Fortran `parallel loop` construct does not require an `end` since the compiler recognizes `end do`.

```c
#pragma acc parallel loop
    for (int i=0; i<n; ++i) {
        a[i] = 0.0;
        b[i] = 1.0;
        c[i] = 2.0;
    }

#pragma acc parallel loop
    for (int i=0; i<n; ++i) {
        a[i]=b[i]+c[i];
    }
```

Fortran programmers should note that the `kernels` directive will handle array operations (much like `workshare` in OpenMP) but `parallel loop` requires an explicit loop.

```fortran

!$acc parallel loop
do i=1,n
    a(i) = 0.0
    b(i) = 1.0
    c(i) = 2.0
end do


!$acc parallel loop
do i=1,n
    a(i) = b(i)+c(i)
enddo

```
