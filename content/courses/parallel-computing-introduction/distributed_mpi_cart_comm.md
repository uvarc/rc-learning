---
title: "MPI Topology Communicators"
toc: true
type: docs
weight: 370
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

We have already [discussed](/courses/parallel-computing/distributed_mpi_higher_dims/) setting up two-dimensional grids of processors.  We set up our own mathematicsl to compute the _topology_, the layout of nearest neighbors as a funciton of row and column.  But we could save ourselves the trouble, and possible errors, by using a built-in feature of MPI called *Topology Communicators*.

We have learned how to create new communicators.  MPI provides a specialized set of routines to generate a communicator with the row and column coordinates computed for us.  As usual, it also can provide the rank relative to the new communicator.

Communication at the ends of the Cartesian block can be periodic or nonperiodic.  So far we've only considered nonperiodic boundaries, where for example the "left" of rank - is `MPI_PROC_NULL1, as is the "right" of the rank at the end of the row.  We could also wrap the domain, so that data from rank 0 goes to rank nrows-1 and vice versa. 

{{< figure src="/courses/parallel-computing-introduction/img/mpi_2d_periodic_bcs.png" alt="Ranks laid out 3x2 with arrows between 0 and 2 and 1 and 5." caption="Periodic boundary conditions for a Cartesian layout." >}}

We will just summarize the types for the subprograms related to Cartesian communciator sytnax.

## C++
```c++
int ndims;
```
dims is a 2-d int array with elements nrows, ncols

periods is a 2-d int array with elements 0 for nonperiodic boundary along the zeroth dimension (rows in our example) and 1 for a periodic boundary along the first dimension (columns in our example).  A periodic boundary wraps the identity of the neighbor to the "empty" side of a rank.  

reorder: allow (1) or not (0) MPI to reorder ranks if necessary.  Often not needed for simple Cartesian grids.

grid_comm: MPI_Comm, the new communicator.

grid_coords: two-dimensional int array whose values are the coordinates of the location of the ranks specified relative to the Cartesian grid of the new communicator.

```c++
   MPI_Cart_create(MPI_COMM_WORLD, ndims, dims, periods, reorder, &grid_comm);
   MPI_Comm_rank(grid_comm, &grid_rank);
   MPI_Cart_coords(grid_comm, grid_rank, ndims, grid_coords);
   MPI_Cart_shift(grid_comm, direction, displ, &up, &down);

```

## Fortran 2008

Other than `periods` and `reorder`, the types are similar to the C++ arguments. If using the optional last `ierror` argument, it is integer as usual.

periods: two-dimensional logical array with values .true. (periodic) or .false.(not periodic).
reorder: logical .true. or .false.

```fortran
  call MPI_Cart_create(MPI_COMM_WORLD, ndims, dims, periods,                 &
                                                     reorder, grid_comm)
   call MPI_Comm_rank(grid_comm, grid_rank)
   call MPI_Cart_coords(grid_comm, grid_rank, ndims, coords)
  call MPI_Cart_shift(grid_comm, direction, displ, up, down)

```

## Python

dimes: two-dimensional list, arguments nrows, ncols as for other languages.
periods: two-dimensional Boolean with arguments True (periodic) or False (nonperiodic) similar to Fortran.
reorder: Boolean True or False.

```python
grid_comm=comm.Create_cart(dims, periods)

grid_rank=grid_comm.Get_rank()
grid_coords=grid_comm.coords
(left,right)=grid_comm.Shift(direction,displ)
```
## Example

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_cart_topo.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_cart_topo.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_cart_topo.py" lang="python" >}}
{{< /spoiler >}}

