---
title: SMP Data Parallelism
date: 2026-07-29T17:26:29Z
type: book 
weight: 1040
menu: 
    parallel_programming:
        parent: Multithreaded Programming
---

In data parallelism, the same operations are performed in some sequence on different parts of the data.  This typically expressed as a loop.

```c++
for (i=0;i<imax;i++) {
   a[i]=b[i]+c[i]
}
```

## A Real-World Example

Building a brick wall in parallel with multiple masons working.

1. Materials are delivered.
2. The foreman assigns the work.
3. Each mason lays brick in his assigned section.
  * Overlap of regions done by different masons must be managed.
4. Smooth joints between sections to make a unified whole.

We can see that for this procedure to work, all the masons must be coordinated. The brick pattern must be the same throughout the wall.  The boundaries must be computed in advance and the appropriate size for each mason's section, and the starting position of the brick layer, must be determined. 

{{< figure src="/courses/parallel-computing-introduction/img/bricklayers.png" caption="Work goes faster with more workers, but they must be coordinated" width=50% alt="Two masons working on a brick wall..">}}

The time saving by laying bricks (or stones, or blocks) in parallel with multiple workers can be the difference between whether a project is feasible or not.

We can make this analogy to both distributed memory programming and shared memory programming.  The main difference is that in distributed-memory programming the programmer is responsible for managing the work, whereas in most multithreading models the higher-level shared-memory programming libraries handle the distribution of work.

## A Computational Example

Find the maximum of a function using a "brute force" method.
1. Evaluate the function at a huge number of randomly-distributed points over a specified range of independent variables.
2. Distribute these points out so that each process evaluates the function throughout the range.
3. Each process computes the maximum of its sample.
4. Individual maxima are returned to the master process, which selects the maximum of maxima as the result.

We have solved this problem using [MPI](/courses/parallel-computing-introduction/distributed_mpi_project_set1). We can also solve it with threading, but we first need to study a threading programming model.  We will start with the most popular for application programs, [OpenMP](https://www.openmp.org/).
