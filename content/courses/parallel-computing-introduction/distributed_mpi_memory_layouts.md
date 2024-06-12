---
title: "MPI Buffers for Halo Exchanges"
toc: true
type: docs
weight: 140
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

Halo exchanges typically involve slices of arrays. In our current example, we are discussing exchanging parts of a two-dimensional array. We have learned how to communicate one-dimensional [arrays](/courses/parallel-computing-introduction/distributed_mpi_array_buffers); now we must consider communicating portions of higer-dimensional arrays.

As we learned, MPI does not construct buffers from variables; it reads a specified number of bytes starting at a specified location in memory.  We must therefore take into consideration how arrays are organized in memory.

In a given programming language, an array can be _row major oriented_ or _column major oriented_.  In a row-major oriented language (most of them, including C/C++ and Python) a two-dimensional array is represented in memory in terms of its indices as

{{< diagram >}}
flowchart LR
   A[0,0] --- B[0,1]
   B --- C[0,2]
   C --- D[0,3]
   D --- E[1,0]
   E --- F[1,1]
   F --- G[1,2]
   G --- H[1,3]
   H --- I[2,0]
   I --- J[2,1]
   J --- K[2,2]
   K --- L[2,3]
   L --- M[3,0]
   M --- N[3,1]
   N --- O[3,2]
   O --- P[3,3]
{{< /diagram >}}

That is, the _second_ index changes most rapidly as we traverse the array in memory.  

In C/C++ the rows may or may not be contiguous in memory, but the elements of each row will be contiguous.

In a column-major oriented language (Fortran and some others such as Matlab, Julia, and R) the layout is by column. These languages also count from 1, at least by default, as represented below.

{{< diagram >}}
flowchart LR
   A[1,1] --- B[2,1]
   B --- C[3,1]
   C --- D[4,1]
   D --- E[1,2]
   E --- F[2,2]
   F --- G[3,2]
   G --- H[4,2]
   H --- I[1,3]
   I --- J[2,3]
   J --- K[3,3]
   K --- L[4,3]
   L --- M[1,4]
   M --- N[2,4]
   N --- O[3,4]
   O --- P[4,4]
{{< /diagram >}}

In this case the _first_ index varies most rapidly.  In Fortran all columns of a two dimensional array (and analogously for higher-dimensional arrays) are guaranteed to be contiguous in memory.

The figure we examined [previously](/courses/parallel-computing-introduction/distributed_mpi_halo_exchange) illustrates an exchange of columns and would be used for the column-major languages. To visualize the exchange for row-major languages, we just rotate that figure 90 degrees.

{{< figure src="/courses/parallel-computing-introduction/img/halo_exchange_row_order.png" caption="We will send edge rows for row-major languages." >}}

