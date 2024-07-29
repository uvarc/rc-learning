---
date: "2020-11-17"
title: "MPI Vector Type Example"
weight: 230
---

Our example will construct an $N \times $M$ array of floating-point numbers.  In C++ and Python we will exchange the "halo" columns using the MPI type, and the rows in the usual way.  In Fortran we will exchange "halo" rows with MPI type and columns with ordinary Sendrecv.
