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

