---
date: "2020-11-17"
title: "Sending and Receiving on a Grid"
weight: 95
---

In the most common cases, we want a full exchange of data between every two communicating processes.  If we examine our schematic again

{{< figure src="/courses/parallel-computing-introduction/img/nearest_neighbor.png" caption="Schematic of nearest-neighbor exchange" >}}

we will observe that we can think of the blue arrows as a sweep from _left_ to _right_, while the orange arrows represent a return sweep from _right_ to _left_. This pattern is particularly common when we are computing on a distributed domain and must exchange information among the subdomains.

## Domain Decomposition

We have discussed [domain decomposition](/courses/parallel-computing-introduction/distributed_mpi_domain_decomp) in a general case.  Let us now consider a more specific example.  We wish to do some computations on a _grid_. A grid divides some region into smaller segments often called _zones_. We will convert our problem from some kind of continuous equations into a _discretized_ system that will be solved on the grid.

The illustration shows a small 10x16 grid that we will divide into subgrids, one for each rank. To keep our first example simple, we will do a one-dimensional decomposition in which we will split the grid into strips.

{{< figure src="/courses/parallel-computing-introduction/img/halo_domain_decomp.png" caption="Dividing a grid into subgrids." >}}
