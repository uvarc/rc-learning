---
date: "2020-11-17"
title: "MPI Derived Types"
weight: 220
---

Modern programming languages provide data structures that may be called "structs," or "classes," or "types."  These data structures permit grouping of different quantities under a single variable name.

MPI also provides a general type that enables programmer-defined datatypes. Unlike arrays, which must be adjacent in memory, MPI derived datatypes may consist of elements in non-contiguous locations in memory.

While more general derived MPI datatypes are available, one of the most commonly used is the `MPI_TYPE_VECTOR`. This creates a group of elements of size _blocklength_ separated by a constant interval, called the _stride_, in memory. Examples would be generating a type for columns in a row-major-oriented language, or rows in a column-major-oriented language.  

{{< figure src="/courses/parallel-computing-introduction/img/mpi_vector_type.png" caption="Layout in memory for vector type. In this example, the blocklength is 4, the stride is 6, and the count is 3." >}}

C++
```c++
MPI_Datatype newtype;
MPI_Type_vector(ncount, blocklength, stride, oldtype, newtype);
```
Fortran
```fortran
integer newtype
!code
call MPI_TYPE_VECTOR(ncount, blocklength, stride, oldtype, newtype, ierr)
```
For both C++ and Fortran, `ncount`, `blocklength`, and `stride` must be integers. The `oldtype` is a pre-existing type, usually a built-in MPI Type such as MPI_FLOAT or MPI_REAL. For C++ the new type would be declared as an `MPI_Datatype`, unless it corresponds to an existing built-in type.  For Fortran `oldtype` would be an integer if not a built-in type. The `newtype` is a name chosen by the programmer.

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

To use our newly committed type in an MPI communication function, we must pass it the starting position of the data to be placed into the type.

C++
```c++
MPI_Send(&a[0][i],1,newtype,i,MPI_COMM_WORLD);
//We need to pass the first element by reference because an array element
//is not a pointer
```

Fortran
```
MPI_Send(a(1)(i),1,newtype,i,MPI_COMM_WORLD,ierr)
```




