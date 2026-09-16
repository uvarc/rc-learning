---
title: OpenACC Data Regions
date: 2026-09-2T17:26:29Z
type: book 
weight: 4080
menu: 
    parallel_programming:
        parent: Accelerator Programming   
---

OpenACC can analyze data movement to and from the device, but cannot always optimize it, and data transfer is often the most time-consuming part of using a GPU. Excessive copies can slow down the execution considerably. For example, with no other information, the compiler will generate copies at each iteration of an inner loop.

```c
while (error>tol && i<maxIter) {
#pragma acc parallel loop reduction(max:err)
    //Data transfer into GPU
    for (something) {do things with arrays A, Anew}
    //Data transfer out of GPU
}
```

The programmer can control this more directly with the `data` directive.
The unmodified `data` directive indicates that data is shared within the region it contains.  It can enclose multiple parallel regions; however, it must begin and end in the same scoping unit (e.g. a subprogram or function).

```c
#pragma acc data
{
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
}
```

```fortran

!$acc data

!$acc parallel loop
do i=1,n
    a(i) = 0.0
    b(i) = 1.0
    c(i) = 2.0
end do
!$acc end parallel loop

do i=1,n
    a(i) = b(i)+c(i)
enddo
!$acc end parallel loop

!$acc end data

```

## Data Clauses

The `data` directive can take clauses that provide more information about data movement to the compiler.  Each of these must be followed by a list of variables enclosed in parentheses.

These clauses can be combined, e.g. 
```c
#pragma acc data create(y) copy(x)
```

### Copy

Copy causes space to be allocated on the device, the data copied at the beginning of the region, then copied back to the host at the end.

```c
#pragma acc data copy (list)
```

```fortran
!$acc data copy (list)
!$acc end data 
```

### Copyin

Copyin causes space to be allocated on the device. The data are copied at the beginning of the region but not copied back to the host at the end.

```c
#pragma acc data copyin (list)
```

```fortran
!$acc data copyin (list)
!$acc end data 
```

### Copyout

Copyout causes space to be allocated on the device but not initialized with a copy from the host. The results are copied back at the end.

```c
#pragma acc data copyout <list>
```

```fortran
!$acc data copyout <list>
!$acc end data 
```

### Create

Create causes space to be allocated on the device for the variables but neither copied in nor copied out.

```c
#pragma acc data create <list>
```

```fortran
!$acc data copyout <list>
!$acc end data 
```

## Array Shaping

In C/C++ an array is typically a pointer or set of pointers so the compiler does not know at compile time its size or shape, and may be unable to optimize the data movement. Thus an extension can be added to the variable in a `data` clause to specify its shape and size explicitly.

```c
#pragma acc data copy(x[0:N])
```

The format is `x[start:count]` where `count` is the _number_ of items. If the start is 0 it can be omitted

```c
#pragma acc data copy(x[:N])
```

Fortran arrays carry information about their shapes and sizes that the compiler can access, so typically the shaping extension need not be used, but when it is it has the format `x[start:end]` with `start` the beginning index and `end` the ending.


```fortran
!$acc data copy(x[1:N])
```
