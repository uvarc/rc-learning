---
title: "MPI Two-Dimensional Exchange"
toc: true
type: docs
weight: 325
date: "2025-08-08T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

We can now use types to illustrate a two-dimensional halo exchange.  For these examples, we will revert to blcoking send and receive to simplify the codes.

## Special Note for Python

As we have [mentioned](/courses/parallel-computing-introduction/distributed_mpi_higher_dims.md), the internal values of a NumPy array are not directly accessibly by MPI.  We can use the Vector type for column 0, but anything else rquires a different approach.  One of the simpler methods is to use another built-in MPI type, the _subarray_.

```python
subarr=MPI.DOUBLE.Create_subarray(sizes,subsizes,starts,order=ORDER_C)
```

The subarray can also be used for C++ and Fortran.

```c++
MPI_Type_create_subarray(int ndims, const int sizes_array, const int subsizes_array, const int starts_array, int order, MPI_Datatype oldtype, MPI_Datatype newtype)
```

```fortran
    INTEGER :: ndims, sizes_array(ndims),subsizes_array(ndims), starts_array(ndims), order
    TYPE(MPI_Datatype):: oldtype
    TYPE(MPI_Datatype):: newtype
    INTEGER :: ierror  ! optional
    !order is usually MPI_ORDER_FORTRAN
    call MPI_Type_create_subarray(ndims, sizes_array, subsizes_array,
                          starts_array, order, oldtype, newtype, ierror)
```

## Example

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_twod_exchange.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_twod_exchange.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_twod_exchange.py" lang="python" >}}
{{< /spoiler >}}

