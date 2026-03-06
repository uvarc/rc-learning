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

**Examples**

These example codes set up and execute one halo exchange. First we 
find the neighbors for each rank ("up" and "down" or "left" and "right") before doing any transfers. Boundary conditions are set, though in these examples they do not represent anything physical.

In order to verify our results, we print the beginning values of the local arrays for each rank.  In order to keep the values organized, all non-root processes will send values to the root process, which will then print them.  This is repeated after the exchange, so we can check that the exchanges are correct. This is not efficient and is not normally done in a production code, but it allows us to understand what happens.  Remember that "real" boundary values are sent into halos (also called ghost zones).

Fortran and NumPy arrays are contiguous in memory, but C/C++ currently lacks true multidimensional, dynamically-sized arrays. Our example C++ code sets up the arrays as an array of pointers, each pointing to a one-dimensional array.  These are not generally consecutive in memory.  For MPI the "buffere is passed as the pointer to the first locations.  If the array is not contiguous, as is usually the case, sending `ncount*size_of_type` will not pull in all the values.  Therefore we pack the two-dimensional array into a one-dimensional array for printing.

In order to receive the buffer from each of the other processes for printing,root will loop through all other ranks, receiving, while the send involves a test to sthat each rank sends once to the root.  We use `MPI_ANY_SOURCE` and `MPI_ANY_TAG` (`MPI.ANY_SOURCE` and `MPI.ANY_TAG` for mpi4py) to match whatever message srrives, since we cannot guarantee ordering.  We can then use the `MPI _Status` variable to extract information about the sender.

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/code/mpi_halo_exchange.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/code/mpi_halo_exchange.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/code/mpi_halo_exchange.py" lang="python" >}}
{{< /spoiler >}}

**Exercise**
Review [strong and weak scaling](performance_analysis.md). The example programs are set up for strong scaling.  Modify them by using constants for the local row and column sizes.

Simplify the validation step by taking two or three appropriate ranks and comparing the before and after values of the exchanged rows or columns. 
