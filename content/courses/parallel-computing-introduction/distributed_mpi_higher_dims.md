---
title: "Higher Dimensional Data Decomposition"
toc: true
type: docs
weight: 310
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

We chose to divide our computational grid into rows for C++ and Python and columns for Fortran because of the memory layouts used by the three languages. But it could be that a one-dimensional decomposition would not be sufficient to allow us to complete the solution in a reasonable amount of time or memory utilization, and we will need a two-dimensional decomposition.

{{< figure src="/courses/parallel-computing-introduction/img/two-d_decomp.png" caption="Two-dimensional halo decomposition. Physical boundaries are not shown." >}}

Since columns in C++ and Python and rows in Fortran are not contiguous in memory, in order to create an MPI buffer we must skip through the elements and copy the appropriate ones into the buffer. 

C++
```c++
  int nrows=nrl+2
  int ncols=ncl+2
  double *buf=new double[nrows];
  for (i = 0; i < nrows; i++ ) {
      [buf[i]=u[i%ncols][0];
  }
```
Python
```python
nrows=nrl+2
ncols=ncl+2
buf=np.zeros(nrows)
for i in range(nrows):
    buf[i]=u[i%ncols,0]
```
Fortran
```fortran
!declare buff allocatable(:)
    nrows=nrl+2
    ncols=ncl+2
    allocate(buf(0:ncols-1)
    do i=0,ncols-1
        buff(i)=u(0,mod(i,nrows))
    enddo
```
This creates the required buffers but involves copying, which can be slow. SOften it is better to use a special MPI feature to pull the buffer directly from memory usin and _MPI type_.

In addition to creating a buffer for the non-contiguous data, we must generate the topology of the rank organization.  For a two-dimensional Cartesian grid, each rank will be assigned a subset of the full grid and we must determine the nearest neighbors by row and column.  We will show an example for Python; C++ and Fortran computations are similar. (Fortran programmers: the percent sign in Python usually corresponds to the `mod` function in Fortran.  C++ and Fortran: integer division returns an integer, whereas in Python it now returns a double so we must explicitly use the integer operator.)

```python
nproc_rows=2
nproc_cols=3

#Set up the topology assuming processes numbered left to right by row
my_row=rank//nproc_cols
my_col=rank%nproc_cols

# setting up the up and down rank for each process
if my_row == 0 :
    up = MPI.PROC_NULL
else :
    up = rank - nproc_cols

if my_row == nproc_rows-1 :
    down = MPI.PROC_NULL
else :
    down = rank + nproc_cols

# left and right
if my_col == 0 :
    left = MPI.PROC_NULL
else:
    left = rank-1

if my_col == nproc_cols-1:
    right = MPI.PROC_NULL
else:
    right = rank+1
```
