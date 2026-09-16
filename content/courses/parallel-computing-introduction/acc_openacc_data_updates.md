---
title: OpenACC Data Updates
date: 2026-09-2T17:26:29Z
type: book 
weight: 4095
menu: 
    parallel_programming:
        parent: Accelerator Programming   
---

## Update

It is optimal to keep data in the device as long as possible without copying, but sometimes a synchronization between the host and device is required.  For that we can use the `update` directive. It requires a clause `device` to copy from the host to the device, or `self` to update from device memory to host memory.

```c
#pragma acc update self(A[0:N])
#pragma acc update device(B[0:N])
```

```fortran
!$acc update self(A[1:N])
!$acc update device(B[1:N])
```

## Unstructured Data Persistence

Our previous data region directive required the data to be allocated within the program scope.  But many programs, especially those written in an Object-Oriented style in C++, allocate and potentially deallocate data in a class, with the data accessible only within the class. Similarly, Fortran codes also frequently allocate and deallocate variables within a module/class.  For these applications we can use `enter` and `exit`.

### enter data

The `enter data` directive takes `create` and `copyin` clauses to indicate when data should be created and copied to the device.

This example is lines that are added to a class constructor in C++ to set up the data on the device. The `this` pointer makes sure that private members are accessible.

```c
#pragma acc enter data copyin(this)      
#pragma acc enter data create(A[0:N])
```

The destructor then contains

```c
#pragma acc exit data delete(A)
#pragma acc exit data delete(this)
```

When the data are changed after an `enter` the `update` directive should be used.
