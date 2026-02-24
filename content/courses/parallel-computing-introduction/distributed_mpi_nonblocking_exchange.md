---
title: "Nonblocking Halo Exchange Example"
toc: true
type: docs
weight: 230
date: "2024-08-05T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

Our first application of nonblocking point-to=point communications is to the famiamilar halo exchange problem.  We can modify the solutions to the [Exercise](/courses/parallel_computing_intro/distributed_mpi_sendrecv.md), 

We can easily convert the `Sendrecv` invocations to two `Irecv` and two `Isend` calls.  With nonblocking sends and receives, we do not have to carefully manage the "sweeps" up and down or right to left; the MPI libary will handle that.  This avoids the serialization of the blocking exchanges; this was the main purpose for the introduction of nonblocking communications.  

As a general rule, the calls to `Irecv` should be posted first, followed by the `Isend`.  In addition, in many cases such as this, it is preferable to use `Waitall` so that the MPI library can handle the ordering of the completions.

{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_nonblock_halo.cxx" lang=c++ >}}

{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_nonblock_halo.f90" lang=fortran >}}

{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_nonblock_halo.py" lang=python >}}


