---
title: "MPI Vector Type Example"
toc: true
type: docs
weight: 230
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

Our example will construct an $N \times $M$ array of floating-point numbers.  In C++ and Python we will exchange the "halo" columns using the MPI type, and the rows in the usual way.  In Fortran we will exchange "halo" rows with MPI type and columns with ordinary Sendrecv.
