---
date: "2020-11-17"
title: "Receive Status"
weight: 78
---

The _status_ variable contains information about the message.

* MPI_SOURCE
    The source rank of the message. This is a field of the status structure.
    * C++
        status.MPI_SOURCE
    * Fortran
        status(MPI_SOURCE)
    * Python
        status.Get_Source()
        
* MPI_TAG
    The tag. This is another field of the structure (or element of the array).
    * C++
        status.MPI_TAG
    * Fortran
        status(MPI_TAG)
    * Python
        status.Get_tag()

* MPI_Get_count(MPI_Status\* status, MPI_Datatype datatype, int\* count)
    The length (item count) of the message.  
    * C++
        MPI_Get_count(&status,MPI_TYPE,&item_count);
    * Fortran
        MPI_Get_count(status,MPI_TYPE,item_count,ierr)
    * Python 
        status.Get_count(MPI.TYPE)

* MPI_Error
    The error number.  Not often needed.
    * C++
        status.MPI_ERROR
    * Fortran
        status(MPI_ERROR)
    * Python
        status.Get_error()

The MPI_SOURCE and MPI_TAG items may be especially useful for the special dummy variables defined for source and tag.

## Special Source and Tag Variables

MPI defines special variables that can be used in MPI_Recv
```no-highlight
MPI_ANY_SOURCE
MPI_ANY_TAG
```
Either or both can be used in a receive if the MPI_Recv can accept a message from any source and/or any tag.

**Example Status Usage**

Add a status query to the exchange code to find the source, tag, and any error.  Note that Python uses functions to retrieve all the members of the status structure, and must also initialize it with a constructor.

Although the status data structure can also return the item count, if the receiver does not know the number or size of the message, we recommend invoking `MPI_PROBE`, which allows the receiving process to determine the specifics of the message without actually receiving it. It can then set up an appropriate MPI_Recv.

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/send_recv_stat.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/send_recv_stat.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/send_recv_stat.py" lang="python" >}}
{{< /spoiler >}}
