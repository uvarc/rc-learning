---
title: "MPI Project Set 4"
toc: true
type: docs
weight: 450
date: "2026-04-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

## Project 1

Many algorithms can or must utilize more than one ghost zone in each dimension.  Write a program in the language of your choice to send and receive the halo zones between two processes. You may assume the number of ghost zones is the same in each dimension.  You do not have to use subarrays but they may simplify the code.

## Example Solutions

### C++

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_ghostzones.cxx" lang="c++" >}}
{{< /spoiler >}}

### Fortran

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_ghostplate.f90" lang="fortran" >}}
{{< /spoiler >}}

### Python

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_ghostzones.py" lang="python" >}}
{{< /spoiler >}}

## Project 2

2. Modify or write a two-dimensional Jacobi solver with a variable number of ghost zones in each dimension.  As in (1) you may assume the same number of ghost zones in each dimension.  Use subarrays for the halo communications.  Make your sends and receives nonblocking.

## Example Solutions

### C++

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_ghostplate.cxx" lang="c++" >}}
{{< /spoiler >}}

### Fortran

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_ghostplate.f90" lang="fortran" >}}
{{< /spoiler >}}

### Python

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_ghostplate.py" lang="python" >}}
{{< /spoiler >}}
