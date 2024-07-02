---
title: "Sending and Receiving with Halo Exchanges"
toc: true
type: docs
weight: 150
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

From our examination of the illustrations for column-major and row-major languages, we conclude that we should split our two-dimensional grid by _columns_ for Fortran and similar languages, and by _rows_ for C++, Python, and others with that array orientation. 

Let us assume for both cases that we have `nr` rows and `nc` columns in our grid.  In a one-dimensional decomposition, we will split only one of those values among the processes.  Call the _local_ number of rows and columns `nrl` and `ncl`. When boundary ("ghost" or real) are added, each subdomain will contain `nrl+2` and `ncl+2` rows and columns. Our computed domains will extend from $1$ to nrl and 1 to ncl, with boundaries at $0$ and at $nrl+1$ and $ncl+1$.  Two exchanges will require two Sendrecv invocations.

## C++ and Python

For these languages $nrl = nr \div nprocs$ and $ncl=nc$. We will send the first computed row _up_ to row $nrl+1$ of the process at $rank-1$ and the last computed row _down_ to row $0$ of the process at $rank+1$.  

C++ syntax:
```C++
MPI_Sendrecv(&w[1][1],nc, MPI_DOUBLE,up,tag,&w[nrl+1][1],
                      nc, MPI_DOUBLE,down,tag,MPI_COMM_WORLD,&status);

MPI_Sendrecv(&w[nrl][1],nc,MPI_DOUBLE,down,tag,&w[0][1],
                        nc,MPI_DOUBLE,up,tag,MPI_COMM_WORLD,&status);
```
Note that although the variable `w` alone would be a pointer, a specific element is not (it dereferences the memory location) and so must be passed to MPI by reference in C++.

Python syntax:
```python
comm.Sendrecv([w[1,1:nc+1],MPI.DOUBLE], up, tag, [w[nr+1,1:nc+1],MPI.DOUBLE], down, tag )
comm.Sendrecv([w[nr,1:nc+1],MPI.DOUBLE], down, tag, [w[0,1:nc+1],MPI.DOUBLE], up, tag )
```

## Fortran

For Fortran $nrl=nr$ and $ncl= nc \div nprocs$. We send the first computed column _left_ to column $ncl+1$ of $rank+1$ and the last computed column _right_ to column $0$ of $rank-1$.

```fortran
call MPI_SENDRECV(w(1:nr,1),nr,MPI_DOUBLE_PRECISION,left,tag, w(1:nr,ncl+1),nr,&
                               MPI_DOUBLE_PRECISION,right,tag,                 &
                                                  MPI_COMM_WORLD,mpi_stat,ierr)

call MPI_SENDRECV(w(1:nr,ncl),nr,MPI_DOUBLE_PRECISION,right,tag, w(1:nr,0),nr, &
                                 MPI_DOUBLE_PRECISION,left,tag,                &
                                                  MPI_COMM_WORLD,mpi_stat,ierr)
```

### MPI_PROC_NULL

Rank $0$ has no neighbor to the top or left, and rank $nprocs$ has no neighbor to the bottom or right. We might think that we would have to write a complicated set of conditionals to handle these situations, but this is a common scenario and MPI provides a built-in solution: `MPI_PROC_NULL`.

The special value MPI_PROC_NULL can be used in place of a source or destination and results in a "no op," i.e. the send or receive is not attempted and the function returns immediately.

**Exercise**

Use the language of your choice.  Write a program that fills an array w with the uniform value of `50` in the inner grid (1 to nr, 1 to nc) where nr=nc=500.  Set the values of row 0 to 0 and the values of row nrl+1 and columns 0 and ncl+1 to 100.  Do not worry about whether values at corners match. 

Set up the transfer and run one exchange from u to w. Print some values to check your result.

Review [strong and weak scaling](performance_analysis.md).  Try both with different runs in your code. For strong scaling, make sure that the number of processes equally divides the number of rows or columns. 

_Hint_
Set up the neighbors for each rank ("up" and "down" or "left" and "right") before doing any transfers.

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_halo_exchange.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_halo_exchange.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_halo_exchange.py" lang="python" >}}
{{< /spoiler >}}

