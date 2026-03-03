---
title: "Splitting MPI Groups and Communicators"
toc: true
type: docs
weight: 350
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

In our previous examples, we defined a variable `flag` for C++ and Fortran.  This variable labeled the process as even or odd as it was divided into the appropriate group.  We can use a built-in MPI routine that uses such a labeling to bypass the need to create the new group or groups explicitly in order to set up our communicators. This routine, `MPI_COMM_SPLIT`, splits the original communicator into subcommunicators based on a labeling of the processes called _color_ and _key_.  The _color_ sorts the processes into communicators, while the _key_ specifies the rank orderint relative to the new communicator.

{{< figure src="/courses/parallel-computing-introduction/img/mpi_comm_split.png" caption="We can split a communicator into non-overlapping subcommunicators." >}}

The MPI_Comm_split routine splits the entire original group.  Each process will belong to one unique subcommunicator, even though the name ("new_comm") will be the same for each.  If we do not want all processes to be a member of some subcommunicator, we can set the "color" to be `MPI_UNDEFINED`.  The process "new_comm" will then have the value `MPI_COMM_NULL`.

The key specifies the order, not necessarily the value, of the rank relative to the new communicator.  Hence if we use the rank in the original communicator, the ordering of the new ranks will be the same as their order to one another in the parent coummunicator.  If we do not care about speciying rank order in the new communicator at all, we can pass a value of `keys` equal to zero and MPI will assign the ranks in the same order as in the parent.

```c++
int MPI_Comm_split(MPI_Comm oldcomm, int color, int key, MPI_Comm *newcomm)
```

```fortran
MPI_Comm_split(old_comm, color, key, new_comm, ierror)
     TYPE(MPI_Comm), INTENT(IN) :: old_comm
     INTEGER, INTENT(IN) :: color, key
     TYPE(MPI_Comm), INTENT(OUT) :: new_comm
     INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```

```python
new_comm=comm.Create()
new_comm.Split(color=0, key=0)
```
## Exercise

Rewrite the group-creation exercise to use `MPI_Comm_split`.

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_comm_split.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_comm_split.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_comm_split.py" lang="python" >}}
{{< /spoiler >}}

