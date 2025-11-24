---
title: "MPI Two Diemnsional Halo Exercise"
toc: true
type: docs
weight: 375
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

We can now pull all of our new knowledge together to let MPI do more of the work for us in the heated-plate problem.

**Exeercise**

In the language of your choice, write a heated-plate solution using two-dimensional data decomposition. Use MPI types and the MPI Cartesian communicator. Stay with blocking `sendrecv` for this exercise.  Boundary conditions should be nonperiodic in both directions.

Check your rsults by plotting.  If you wish, you can use the script contour2d.py

{{< spoiler >}}
{{< code-download file=/courses/parallel-computing-introduction/scripts/contour2d.py" lang="python" >}}
{{< /spoiler >}}

The usage is
```no-highlight
python contour2d.py -r numrows -c numcols basefilename
```
The files should be written by row with the name fitting the pattern
```no-highlight
base<coord[0]coord[1]>
```
(or coord[1]coord[2] for Fortran)
e.g. 
```
out00 out01 out11 ...
```

## Example Solutions

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_cartplate.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_cartplate.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_cartplate.py" lang="python" >}}
{{< /spoiler >}}

