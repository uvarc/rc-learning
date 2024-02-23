---
title: "Halo Exchanges"
toc: true
type: docs
weight: 135
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

In our example, suppose that we have a two-dimensional array `A` that must be computed over the entire (global) grid.  Each subgrid will compute a portion of A, but the boundaries must be exchanged. We match each "ghost" zone to a zone on the edge of the grid managed by the neighbor processes.

Our array will be 10x16 globally, and 10x4 on each of 4 ranks. Let us extend each grid so that the array indices are numbered 0 to 11 and 0 to 17.  (Note to Fortran programmers: C++ and Python always count from 0 whereas the default for Fortran is to start from 1, but Fortran can set the lower bound to any integer less than the upper bound.)  This numbering scheme isn't necessary and is not always possible, but it may make the example clearer and simplifies the coding.

More generally, consider a grid of size (0,M+1) by (0,N+1). We will consider the boundaries to be in the 0-indexed rows and columns, and the M+1 or N+1 rows or columns. (Corners may need to be set specially, depending on the problem, but we'll ignore that for now.)  

When we split the array into four parts we now have grids of size (0,11) by (0,5).  Only the columns need to exchange data, since the values of the tops and bottoms are set by the model we are computing.

{{< figure src="/courses/parallel-computing-introduction/img/halo_exchange.png" caption="The halos overlap the edges of their neighbors." >}}

Rank 0 will send its "real" values in Column 4 to the "ghost" values on Rank 1 in Column 0.  Rank 1 sends its computed value in Column 1 to Column 5 in Rank 0, and receives the value of Rank 2, Column 0 into its Column 4. Similarly for Ranks 2 and 3.  Note that the boundaries of Column 0, Rank 0 and Column 5, Rank 3, as well as Rows 0 and 11 on all ranks, are set by the conditions of the model and are not indicated in the diagram.

