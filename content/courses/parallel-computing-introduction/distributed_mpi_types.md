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
```fortran
integer :: ierr
integer :: count, blocklength, stride
integer :: newtype
!code
call MPI_TYPE_VECTOR(ncount, blocklength, stride, oldtype, newtype, ierr)
```
Fortran 2008
```fortran
use mpi_f08
integer :: ierr
integer :: count, blocklength, stride
type(MPI_Datatype) :: newtype
!code
call MPI_Type_vector(count, blocklength, stride, oldtype, newtype, ierr)
```
For both C++ and Fortran, `ncount`, `blocklength`, and `stride` must be integers. The `oldtype` is a pre-existing type, usually a built-in MPI Type such as MPI_FLOAT or MPI_REAL. For C++ the new type would be declared as an `MPI_Datatype`, unless it corresponds to an existing built-in type.  For Fortran the types  would be an integer if not a built-in type, whereas for Fortran 2008 they are a `type` declared similarly to the C++ equivalent. The `newtype` is a name chosen by the programmer.

Python
```python
newtype = oldtype.Create_vector(ncount, blocklength, stride)
```

## Committing the Type

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

## Using a Type

To use our newly committed type in an MPI communication function, we must pass it the starting position of the data to be placed into the type.  Notice that the item count is the _number of instances_ of the type.  In our examples this will usually be 1.

C++
```c++
//We need to pass the first element by reference because an array element
//is not a pointer
MPI_Send(&u[0][i],1,newtype,dest,tag,MPI_COMM_WORLD);
MPI_Recv(&w[0][j],nrl,newtype,source,tag,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
```

Fortran

Note that we do not use array slicing in this example. It is often best to avoid that, especially when using nonblocking communications, because a slice involves a copy.
```fortran
MPI_Send(u(i,1),1,newtype,dest,tag,MPI_COMM_WORLD,ierr)
MPI_Recv(w(j,1),ncl,newtype,source,tag,MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierr)
```

Python

Python NumPy arrays do not generally expose the underlying pointers to the values. We could perhaps create a view (slice), but that would copy the data, which is what we want to avoid. Therefore, we should create the MPI Vector type as above, then use a method such as `frombuffer` to enable MPI to access the raw values.  We also need to provide `frombuffer` with an offset _in bytes_ to tell it where in memory to start reading the values.
```python
#define i and j somewhere
sendcol = i
recvcol = j
comm.Send([np.frombuffer(u.data,np.float,offset=sendcol*np.dtype('float').itemsize),1,newtype],dest)
comm.Recv([w[0:j],MPI.FLOAT,source)
```
