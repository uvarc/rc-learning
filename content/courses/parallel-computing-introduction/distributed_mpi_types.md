---
title: "MPI Derived Types"
toc: true
type: docs
weight: 220
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

Modern programming languages provide data structures that may be called "structs," or "classes," or "types."  These data structures permit grouping of different quantities under a single variable name.

MPI also provides a general type that enables programmer-defined datatypes. Unlike arrays, which must be adjacent in memory, MPI derived datatypes may consist of elements in noncontiguous locations in memory.

While more general derived MPI datatypes are available, one of the most commonly used is the `MPI_TYPE_VECTOR`. This creates a group of elements separated by a constant interval, called the _stride_, in memory. Examples would be generating a type for columns in a row-major-oriented language, or rows in a column-major-oriented language.  

C++
```c++
MPI_Type_vector(ncount, blocklength, stride, oldtype, newtype);
```
Fortran
```fortran
call MPI_TYPE_VECTOR(ncount, blocklength, stride, oldtype, newtype, ierr)
```
For both C++ and Fortran, `ncount`, `blocklength`, and `stride` must be integers. The `oldtype` is a pre-existing type, usually a built-in MPI Type such as MPI_FLOAT or MPI_REAL. For C++ it would be declared as an `MPI_Datatype`, but if built-ins are used that would be automatic.  For Fortran `oldtype` would be an integer if not a built-in type. The `newtype` is a name chosen by the programmer.

Python
```python
newtype = oldtype.Create_vector(ncount, blocklength, stride)
```

A derived type must be _committed_ before it can be used.

```c++
MPI_Type_commit(newtype)
```
Fortran
```fortran
call MPI_TYPE_COMMIT(newtype,ierr)
```
Python
```
newtype.Commit()
```


