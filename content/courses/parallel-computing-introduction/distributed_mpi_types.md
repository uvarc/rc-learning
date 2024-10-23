---
title: "MPI Derived Types"
toc: true
type: docs
weight: 320
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

Modern programming languages provide data structures that may be called "structs," or "classes," or "types."  These data structures permit grouping of different quantities under a single variable name.

MPI also provides a general type that enables programmer-defined datatypes. Unlike arrays, which must be adjacent in memory, MPI derived datatypes may consist of elements in non-contiguous locations in memory.

## Example: MPI_TYPE_VECTOR

While more general derived MPI datatypes are available, one of the most commonly used is the `MPI_TYPE_VECTOR`. This creates a group of elements of size _blocklength_ separated by a constant interval, called the _stride_, in memory. Examples would be generating a type for columns in a row-major-oriented language, or rows in a column-major-oriented language.  

{{< figure src="/courses/parallel-computing-introduction/img/mpi_vector_type.png" caption="Layout in memory for vector type. In this example, the blocklength is 4, the stride is 6, and the count is 3." >}}

C++
```c++
int ncount, blocklength, stride;
MPI_Datatype newtype;
// Note that oldtype is not passed by reference but newtype is
MPI_Type_vector(ncount, blocklength, stride, oldtype, &newtype);
```

Fortran
The `ierror` parameter is optional if the mpi_f08 module is used.
```fortran
integer :: count, blocklength, stride
integer :: newtype
!code
call MPI_TYPE_VECTOR(ncount, blocklength, stride, oldtype, newtype [, ierror])
```
Fortran 2008
```fortran
use mpi_f08
integer :: count, blocklength, stride
type(MPI_Datatype) :: newtype
!code
call MPI_Type_vector(count, blocklength, stride, oldtype, newtype)
```
For both C++ and Fortran, `ncount`, `blocklength`, and `stride` must be integers. The `oldtype` is a pre-existing type, usually a built-in MPI Type such as MPI_FLOAT or MPI_REAL. For C++ the new type would be declared as an `MPI_Datatype`, unless it corresponds to an existing built-in type.  For Fortran the types  would be an integer if not a built-in type, whereas for Fortran 2008 they are a `type` declared similarly to the C++ equivalent. The `newtype` is a name chosen by the programmer.


Python
```python
newtype = oldtype.Create_vector(ncount,blocklength,stride)
```

## Committing the Type

A derived type must be _committed_ before it can be used.

```c++
MPI_Type_commit(newtype)
```
Fortran
```fortran
call MPI_TYPE_COMMIT(newtype[,ierr])
```

Python
```python
newtype.Commit()
```

## Using a Type

To use our newly committed type in an MPI communication function, we must pass it the starting position of the data to be placed into the type.  Notice that the item count is the _number of instances_ of the type.  In our examples this will usually be 1.

C++
```c++
//We need to pass the first element by reference because an array element
//is not a pointer
MPI_Isend(&u[0][i],1,newtype,dest,tag,MPI_COMM_WORLD,&mpi_requests[1]);
MPI_Irecv(&w[0][j],1,newtype,source,tag,MPI_COMM_WORLD,&mpi_requests[2]);
```

Fortran

Note that we do not use array slicing in this example. It is often best to avoid that, especially when using nonblocking communications, because a slice involves a copy.
```fortran
! assuming mpi_f08 module
MPI_Isend(u(i,1),1,newtype,dest,tag,MPI_COMM_WORLD,mpi_requests(1))
MPI_Irecv(w(j,1),1,newtype,source,tag,MPI_COMM_WORLD,mpi_requests(2))
```

Python

Python NumPy arrays do not generally expose the underlying pointers to the values. The `frombuffer` method of NumPy will allow us to set the pointer by using the offset argument.

```python
sendCol = 2
send_request=comm.Isend([np.frombuffer(a.data,intc,offset=sendCol*np.dtype('intc').itemsize),1,cols],dest)
```

## Freeing Types

When we are done with a type, we should free it.

C++
```c++
MPI_Type_free(newtype)
```

Fortran
```fortran
call MPI_Type_free(newtype[,ierror])
```

Python
```python
newtype.Free()
```
