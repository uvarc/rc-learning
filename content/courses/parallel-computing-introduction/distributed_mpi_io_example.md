---
title: "MPI IO Example"
toc: true
type: book
weight: 520
date: "2026-06-10T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

We have covered only a few of the many MPI IO procedures available, but enough to write a working example so we can see how they fit together.  We will assign a single-character upper-case letter, starting with `A`, to each process.  We will have each process write its character to a file for N repetitions.  So in particular, if we have 4 processes and we ask for 3 repetitions, the file should contain
```
ABCDABCDABCD
```
Each character occupies a byte (so for Python we will use the MPI_BYTE type) which simplifies computing the offset.  We will use MPI_File_write_at since this is a small write, and the overhead of a collective write might be excessive.

We will then have each process read the file created and print it to verify that it is as expected.

We can illustrate the file layout with offsets

{{< figure src="/courses/parallel-computing-introduction/img/mpi_write_layout.png" alt="File layout diagram with offsets for the MPI Write example" caption="Each process writes its assigned letter with offset depending on rank and number of processes" >}}

The offset is 4 bytes for each processes and repetition. If we loop over a variable `i`, then some thought shows that the offset for each process is
```
offset=rank+(i-1)*nprocs
```
where `i` is the loop index over the number of repetitions.

**Examples for each language**

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_io_example.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_io_example.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_io_example.py" lang="python" >}}
{{< /spoiler >}}
