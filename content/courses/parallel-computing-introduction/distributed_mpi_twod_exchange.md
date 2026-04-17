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

As we have [mentioned](/courses/parallel-computing-introduction/distributed_mpi_types/), the internal values of a NumPy array are not directly accessibly by MPI.  We can use the Vector type for column 0, but anything else rquires a different approach.  One of the simpler methods is to use another built-in MPI type, the _subarray_. The basic syntax is

```python
subarr=MPI.DOUBLE.Create_subarray(sizes,subsizes,starts,order=ORDER_C)
```
where `sizes` is an ordered type (such as a list) of the size along each dimension, subsizes is the size of the subarray to be selected, starts is the array of starting locations (zero based) for the subarray, and order is whether to use row-major (C) order or column-major (Fortran) order; `ORDER_C` is the default and is optional.  In particular we can define

```python
column_zero=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl,1],[1,0])
column_zero.Commit()

column_one=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl,1],[1,1])
column_one.Commit()

column_end=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl,1],[1,ncl])
column_end.Commit()

column_endbc=MPI.DOUBLE.Create_subarray([nrl+2,ncl+2],[nrl,1],[1,ncl+1])
column_endbc.Commit()

We then send and receive with syntax such as
comm.Recv([u,MPI.DOUBLE],source=MPI.ANY_SOURCE,tag=MPI.ANY_TAG,status=status)
comm.Send([w,MPI.DOUBLE],dest=0,tag=rank)
```

The subarray can also be used with C++ and Fortran, as we will describe in [a later section](/courses/parallel-computing-introduction/distributed_mpi_subarrays).

## Examples

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_twod_exchange.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_twod_exchange.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_twod_exchange.py" lang="python" >}}
{{< /spoiler >}}

