---
title: "Domain Decomposition"
toc: true
type: docs
weight: 45
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

Parallel programs must break down something to distribute among the different processes or threads.  In our [previous](/courses/parallel-computing-introduction/parallel_basics) discussion we talked generally about _task parallelism_ and _data parallelism_, but we have seen few concrete examples applied to programming.  One of the most common parallelisms is a form of data decomposition called _domain decomposition_.  This is typically used for data that can be represented on some form of grid.  We break the grid down into subgrids, and assign each subgrid to a process rank.  There is no hard and fast rule for numbering the subgrids and usually multiple options are possible.

For a concrete example, consider a two-dimension grid of points, with a function defined at each point.  Consider a simple case of four processes. Two obvious possible rank assignments are left-to-right or cyclic.

{{< figure src="/courses/parallel-computing-introduction/img/core_numbering.png" caption="Possible rank assignment on a two-dimensional grid." >}}

How to assign the numbers to the subdomains may depend on the problem. MPI provides some utilities that may help with this, but using them requires creating a new communicator group, which we have not discussed, so for now we will do our ordering manually.

Let us consider the "brute force" maximum problem of [Project Set 1](/courses/parallel-computing-introduction/distributed_mpi_project_set1).  In that example, we randomly evaluated a function over a two-dimensional set of $x$ and $y$ values and used gathers to find an approximation to the value and location of the maximum.  Another approach is to divide up the grid and perform our random searches indepdently over each subgrid, then collect results from each subdomain and compare them to find the maximum and its location.  

A little thought and experimentation suggests that the simplest way to map ranks to subdomains is to divide the overall grid into $np$ regions, where $np$ is the number of processes, then label each domain by row and column number, starting the counts from $0$.  The number of processes must be a multiple of two integers.  In our example we will assume we will use a perfect square, so our grid is $\sqrt{np}$ on each side.  This is not required, but if a more general breakdown is desired, the program must be provided the number of rows and columns and the number of processes should be confirmed to be $n_{rows} \times n_{cols}$. Each process rank then computes its location in the grid according to
```python
   col_num=rank%ncols
   row_num=rank//nrows
```
using Python syntax.  The local column number is the remainder of the rank divided by the number of columns, and the local row number is the integer division of the rank by the number of rows.  This layout corresponds to a rank assigment such as the following, for nine processes:

{{< figure src="/courses/parallel-computing-introduction/img/domain_decomposition.png" caption="Domain decomposition for a typical two-dimensional distribution by rank." >}}

With this decomposition, each rank evaluates the same number of random points, so this is an example of weak scaling.

**Exercise**

If you have not worked the example from Project Set 1, download the sample serial codes referenced there.

Implement the domain decomposition method to solve this problem.  The basic method does not take too many lines of code.  The example solutions also check that the number of processors is a perfect square, but you may assume that for a first pass.  For correct statistical properties you should also attempt to use different random seeds on different ranks.  Be sure not to end up with a seed of $0$ on rank $0$.

Hint: From $xlo$, $xhi$, and $ncols$, compute the increment for each block in $x$ values.  Repeat for $y$ values using $ylo$, $yhi$, and $nrows$.  Compute the location in rows and columns for each rank 
```python
col_num=rank%ncols
row_num=rank//nrows
```
You can now compute the starting and ending $x$ and $y$ values for each rank.

#### Example Solutions
{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpifind_max_dd.cxx" lang="c++" >}}
{{< /spoiler >}}
{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpifind_max_dd.f90" lang="fortran" >}}
{{< /spoiler >}}
{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpifind_max_dd.py" lang="python" >}}
{{< /spoiler >}}


