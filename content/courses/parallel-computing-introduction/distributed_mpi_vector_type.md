---
title: "MPI Vector Type"
toc: true
type: docs
weight: 330
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

Let us examine a specific application of the MPI Vector type.  On each process, we will generate an identical matrix as the source, and another matrix of the same size, initialized to zeros, as the destination.  A process will send the column (for C and Python) or row (Fortran) corresponding to its rank, to the next-higher indexed column or row in the next-higher rank; except for the last rank which will send its colunn/row to the first (zeroth) column/row of rank 0, i.e. the exchanges are cyclic.

## C/C++

C, C++, Python, and many other languages are _row-major oriented_.  That is, a two-dimensional array is mapped to linear memory by stacking one row after another, in order.  In this illustration, we are going to place the zeroth column into a buffer. 

{{< figure src="/courses/parallel-computing-introduction/img/mpi_vector_type_C.png" caption="count=3, blocklength=1, stride=4" >}}

We start the selection at `u[0][0]`.  The count, the number of items, is the number of rows, while the stride is the number of columns.

Since we are receiving into a column, we can use the same type to receive. The count, blocklength, and stride would be the same, but the starting location of the buffer might be different. The important invocations will look like this:

C++
```c++
   //The length of the column is the number of rows
    int ncount=nr;
    //The number of items picked from each stride is 1
    int blocklength=1;
    //The length of the row is the number of columns
    int stride=nc;

    MPI_Datatype cols;
    MPI_Type_vector(ncount,blocklength,stride,MPI_DOUBLE,&cols);
    MPI_Type_commit(&cols);

    MPI_Irecv(&w[0][0], 1, cols, src, tag, MPI_COMM_WORLD, &requests[0]);
    MPI_Isend(&u[0][0], 1, cols, dest, tag, MPI_COMM_WORLD, &requests[1]);
```

{{< spoiler text="The full code in C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_vector_type.cxx" lang="c++" >}}
{{< /spoiler >}}

## Python

As we have [discussed](/courses/parallel-computing-introduction/distributed_mpi_types), when sending MPI types we must extract the raw data from a NumPy array.

```python
#The length of the column is the number of rows
ncount=nr
#The number of items picked from each stride is 1
blocklength=1
#The length of the row is the number of columns
stride=nc

cols = MPI.DOUBLE.Create_vector(ncount, blocklength, stride)
cols.Commit()
recv_request=comm.Irecv([np.frombuffer(w.data,np.double,offset=sendcol*np.dtype('double').itemsize),1,cols],src)
send_request=comm.Isend([np.frombuffer(u.data,np.double,offset=recvcol*np.dtype('double').itemsize),1,cols],dest
```

{{< spoiler text="The full code in Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_vector_type.py" lang="python" >}}
{{< /spoiler >}}

## Fortran

Fortran is _colum-major oriented_.  Memory is laid out by columns. In this illustration, we are selecting the fourth row to place into the buffer.  

{{< figure src="/courses/parallel-computing-introduction/img/mpi_vector_type_fortran.png" caption="count=4, blocklength=1, stride=3" >}}

The buffer will start at `u(3,1)`.  As for C++ and Python, we can use the same vector type for sending and receiving.

Our code snippet would look something like this.
```fortran
! The length of the row is the number of columns
   ncount=nc
   ! The number of items picked from each stride is 1
   blocklength=1
   ! The length of the column is the number of rows
   stride=nr

   call MPI_Type_vector(ncount,blocklength,stride,MPI_DOUBLE_PRECISION,rows)

   call MPI_TYPE_COMMIT(rows)

   call MPI_Irecv(w(1,1),1,rows,src,tag,MPI_COMM_WORLD,mpi_requests(1))
   call MPI_Isend(u(3,1),1,rows,dest,tag,MPI_COMM_WORLD,mpi_requests(2))
```

{{< spoiler text="The full code in Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_vector_type.f90" lang="fortran" >}}
{{< /spoiler >}}
