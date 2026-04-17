---
title: "MPI Subarray Type"
toc: true
type: docs
weight: 400
date: "2026-04-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

We used the subarray type for mpi4py in the [two-dimensional exchange example](/courses/parallel-computing-introduction/distributed_mpi_twod_exchange) due to pointer issues.  We can create subarrays for Fortran and C++ as well, and they can be useful if we wish to define larger blocks than a row or column, such as a submatrix within a computational grid.  For example, many algorithms can utilize more than one ghost zone in each dimension.  In Fortran we could use array slices, or in C++ we could create an intermediate buffer, but both these solutions would require a copy, which can be slow. With a derived type such as the subarray, MPI can pluck its buffer directly from memory without the programmer needing to set up another array or slice.

## C/C++

In this example we pick an array of size `3x4` out of the full local array, which has been declared as `nrl,ncl`. We start the selection at `w[2][3]`.  Always be sure that the subsize fits within the full size.

We specify MPI_ORDER_C because we are using the normal row-major layout. 

```c++
    int ndims=2;
    int sizes[]={nrl,ncl};

    int starts[ndims];
    int subsizes[ndims];

    subsizes[0]=3; subsizes[1]=4;

    MPI_Datatype sendtype;

    starts[0]=2; starts[1]=3;
    MPI_Type_create_subarray(ndims,sizes,subsizes,starts,MPI_ORDER_C,MPI_INT,&sendtype);
    MPI_Type_commit(&sendtype);
```
We will send this selection into a receive buffer of the same subsize. The data will be written into a different section of the local array, one that starts at `w[3][2]`. Be sure that the sizes of sending and receiving buffers always match.
```c++
MPI_Datatype recvtype;
starts[0]=3; starts[1]=2;
MPI_Type_create_subarray(ndims,sizes,subsizes,starts,MPI_ORDER_C,MPI_INT,&recvtype);
MPI_Type_commit(&recvtype);
```
When we send the subarray, we must provide the pointer to the first element in the send buffer, and similarly for the receive buffer. How we obtain this may depend on how our array was set up. In our examples throughout this course we have set up two-dimensional arrays in C++ as C-style dynamically-sized arrays of pointers. This means that the array name `w` just represents the first pointer-to-pointer. So we pass the pointer to the first value directly.
```c++
  MPI_Send(&w[0][0], 1, sendtype, 1, tag, MPI_COMM_WORLD);
  MPI_Recv(&w[0][0], 1, recvtype, root, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
```

The full example program sets up a `6x8` local array in each of two processes, then sends the contents of the selection in one process to the different subsection of the other process.

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_subarray.cxx" lang="c++" >}}
{{< /spoiler >}}

## Fortran

Fortran will be very similar to C++ in this case, since row major versus column major is specified when we define the subarray type. When determining the starting points, we must use 0-based array indexing regardless of how our array was declared. MPI will use the starting position and ordering to calculate the elements to be packed into the buffer. 
```fortran
integer :: ndims 
integer, allocatable, dimension(:) :: wsizes,subsizes, starts
type(MPI_Datatype)::sendtype,recvtype

   ! Declare arrays
   ! w to nrl, ncl
   ! wsizes, subsizes, starts to ndims (2 in our examples)

    ! Set up subarrays
    wsizes = [nrl, ncl]                 ! Full array size
    subsizes = [ sub_rows, sub_cols ]   ! Subarray size

    ! Send array
    starts   = [ 2, 3 ]                 ! Starting indices

    ! Subarray type requires counting from 0
    starts=starts-lb

    call MPI_Type_create_subarray(2, wsizes, subsizes, starts, &
                                  MPI_ORDER_FORTRAN, MPI_INTEGER, sendtype)
    call MPI_Type_commit(sendtype)

    ! Receive array
    starts=[ 3,2 ]-lb

    call MPI_Type_create_subarray(2, wsizes, subsizes, starts, &
                                  MPI_ORDER_FORTRAN, MPI_INTEGER, recvtype, ierr)
    call MPI_Type_commit(recvtype)
```
Fortran passes all arrays by reference, i.e. by passing the pointer, so no special treatment is required in the send or receive.

```fortran
   call MPI_Send(w, 1, sendtype, 1, 0, MPI_COMM_WORLD)
   call MPI_Recv(w, 1 , recvtype, root, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
```

The full program is similar to the C++ example; it sets up a `6x8` local array in each of two processes, then sends the contents of the selection in one process to the different subsection of the other process.

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_subarray.f90" lang="fortran" >}}
{{< /spoiler >}}

## Python

The general syntax for Python subarrays is 
```python
sizes=[nrl,ncl]
subsizes=[sub_rows,sub_cols]
starts=[start_row,start_col]
newtype=MPI.INTEGER.Create_subarray(sizes,subsizes,starts)
newtype.Commit()
```

The complete example is similar to those for C++ and Fortran.

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_subarray.py" lang="python" >}}
{{< /spoiler >}}
