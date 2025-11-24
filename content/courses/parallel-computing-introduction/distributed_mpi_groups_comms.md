---
title: "MPI Groups and Commuicators"
toc: true
type: docs
weight: 350
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

We have seen how MPI provides routines to simplify communications among more complex types than those built in to the base programming languages.  MPI also allows the programmer to create custom _groups_ of processes.

Recall that a communicator contains a group of processes, along with required communication contexts that allow messages to be sent among them.  We always start with the communicator `MPI_COMM_WORLD` predefined for us, containing the group of all processes initialized.  We can derive new groups from the "world" group
and collect those groups into corresponding communicators.

We may wish to create custom communicator groups to isolate a set of processes for certain purposes.  For example, we might divide processes into even and odd groups and communicate different data for each set.

{{< figure src="/courses/parallel-computing-introduction/img/mpi_process_groups.png" caption="We can create new groups from an existing group of processes." >}}

## Groups

A *group* is an ordered set of processes.  Within the group, each process has a rank from 0 to $ng-1$, where $ng$ is the group size.  The value of $ng$ must be less than or equal to the total number of processes $np$.

Upon initialization, every MPI program defines the *world* group consisting of all processes started.  The most general routines for creating a new group are `MPI_Group_inc`, which includes processes meeting a criterion specified by the programmer into the group, and `MPI_Group_excl`, which excludes the specified processes.

Once a groupo is created, we can obtain the size of the group, and for a given process we can find its rank within the _group_.  This rank may not be the same as its rank in the `world` group.

In all our syntax examples, we will show the prototypes of the subprograms; variables must be declared appropriately for C/C++ and Fortran.  We will also use exclusively the Fortran 2008 syntax, which assumes we `use mpi_f08` as the module.

```c++
int MPI_Group_incl(MPI_Group group, int n, const int ranks[], MPI_Group *newgroup)
//or
int MPI_Group_excl(MPI_Group group, int n, const int ranks[], MPI_Group *newgroup)
int MPI_Group_size(MPI_Group group, int *ng)
int MPI_Group_rank(MPI_Group group, int *grpRank)
```

```fortran
MPI_GROUP_INCL(group, n, ranks, newgroup, ierror)
!or
MPI_GROUP_EXCL(group, n, ranks, newgroup, ierror)
    TYPE(MPI_Group), INTENT(IN) :: group
    INTEGER, INTENT(IN) :: n, ranks(n)
    TYPE(MPI_Group), INTENT(OUT) :: newgroup
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_GROUP_SIZE(group, ng, ierror)
     TYPE(MPI_Group), INTENT(IN) :: group
     INTEGER, INTENT(OUT) :: ng
     INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_GROUP_RANK(group, grp_rank, ierror)
     TYPE(MPI_Group), INTENT(IN) :: group
     INTEGER, INTENT(OUT) :: grp_rank
     INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```

```python
#fill ranks sequence (usually a list)
new_group=MPI.Group.Incl(ranks)
#or
new_group=MPI.Group.Excl(ranks)
ng=new_group.Get_size()
grp_rank=new_group.Get_rank()
```

Several other routines can be used to construct and to free groups, but we are mainly interested in communicators so we will refer the reader to the MPI [documentation](https://www.mpi-forum.org/docs/mpi-4.1/mpi41-report/node173.htm#Node173) for additional examples.

## Communicators

We are familiar with the `world` communicator MPI_COMM_WORLD, but we can create new communicators.  Normally, our custom communicators are over some group that is a subset or reorganization of the `world` group, so we typically must define a group before we can instantiate a new communicator. A process will have a rank relative to the custom group that in general will be different from its rank in the `world` group.

The inquiry routines we have used for `MPI_COMM_WORLD` can also be used for our new communicator.  Likewise, any MPI routine containng a communicator argument can operate on our new communicator.

```c++
int MPI_Comm_create(MPI_Comm oldcomm, MPI_Group group, MPI_Comm *newcomm)
int MPI_Comm_size(MPI_Comm newcomm, int *comm_size)
int MPI_Comm_rank(MPI_Comm newcomm, int *comm_rank)
int MPI_Bcast(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm new_comm)
```

```fortran
MPI_Comm_create(oldcomm, group, newcomm, ierror)
    TYPE(MPI_Comm), INTENT(IN) :: oldcomm
    TYPE(MPI_Group), INTENT(IN) :: group
    TYPE(MPI_Comm), INTENT(OUT) :: newcomm
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_Comm_size(newcomm, comm_size, ierror)
     TYPE(MPI_Comm), INTENT(IN) :: newcomm
     INTEGER, INTENT(OUT) :: comm_size
     INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_Comm_rank(newcomm, comm_rank, ierror)
     TYPE(MPI_Comm), INTENT(IN) :: newcomm
     INTEGER, INTENT(OUT) :: comm_rank
     INTEGER, OPTIONAL, INTENT(OUT) :: ierror

MPI_Bcast(buffer, count, datatype, root, newcomm, ierror)
    TYPE(*), DIMENSION(..) :: buffer
    INTEGER, INTENT(IN) :: count, root
    TYPE(MPI_Datatype), INTENT(IN) :: datatype
    TYPE(MPI_Comm), INTENT(IN) :: newcomm
    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
```

```python
new_comm=MPI.Comm.Create(group)
comm_size=new_comm.Get_size()
comm_rank=new_comm.Get_rank()
new_comm.Bcast([message,datatype])
```
**Exercise**

Create two communicators, one for odd-numbered ranks and the other for even-numbered ranks.  Broadcast one message to the first group and another to the second group.  Have each process print its rank relative to the world group and its assigned group.

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_comm_bcast.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_comm_bcast.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_comm_bcast.py" lang="python" >}}
{{< /spoiler >}}

