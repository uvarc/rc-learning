---
date: "2020-11-17"
title: "MPI Buffers for Arrays"
weight: 90
---

So far in our examples we have only discussed sending _scalar_ buffers.  In computing, a scalar is a variable that holds only one quantity. The exact meaning of _array_ varies from one programming language to another, but in all cases it refers to a variable that represents several quantities, each of which can be individually accessed by some form of subscripting the array.

To understand array communications we must consider how MPI buffers are created. The first argument to a send or receive is a _pointer_ to a location in memory.  In C++ this is explicit; the argument must be passed by reference if it isn't declared a pointer.  Fortran always passes by reference so nothing is required aside from the variable name. The mpi4py bindings arrange for the argument to be a pointer so as for Fortran, only the variable name is required.

For a scalar variable, the pointer is to the location in memory of that variable. For an array, the pointer is to the first element of the section of the array to be sent, which may be all or part of it. The _item count_ is the number of items to be sent.  The MPI type specifies the number of bytes per item.  From this information the MPI library computes the total number of bytes to be put into or received from the buffer. The library reads that number of bytes, starting at the initial memory location.  It pays no attention to any indexing of that string of bytes. This is why it is extremely important that the send and receive buffers match up appropriately; in particular, if the send buffer is longer, in bytes, than the receive buffer, then invalid data can be written to the receiver, or the process may even terminate.

Consider an example where each rank computes an array `u` and sends it to its left into a receive buffer `w`.  We will show the syntax only for `MPI_Sendrecv` since how to break it into individual `Send` and `Recv` if desired should be obvious.

In the C++ example we create the arrays with the `new` operator.
```c++
double* u=new double[nelem]{0};
double* w=new double[nelem]{0};
//Fill u with something
MPI_Sendrecv(u, nelem, MPI_DOUBLE,neighbor,sendtag,
             w, nelem, MPI_DOUBLE,neighbor,recvtag,MPI_COMM_WORLD,&status);
```
Normally in C++ an array variable will be declared a pointer and so it is not passed by reference to the MPI subprograms. 

Fortran and Python are straightforward.
```fortran
! In the nonexecutable part we declare u and w. They can be allocatable or static.
! Fill in u with some values
call MPI_Sendrecv(u,nelems,MPI_DOUBLE_PRECISION,neighbor,sendtag,              &
                  w,nelems,MPI_DOUBLE_PRECISION,neighbor,recvtag,              &
                                                   MPI_COMM_WORLD,status,ierr)
```

Python
```
u=np.zeros(nelems)
w=np.zeros(nelems)
#fill in u with some values
comm.Sendrecv([u,MPI.DOUBLE],neighbor,0,[w,MPI.DOUBLE],neighbor,0,MPI.Status())
```

**Exercise**

Use the above syntax for your language to write a complete program to implement the sending and receiving as specified above.  For `u` you should fill it with 
```nohighlight
u[i]=20.+i*rank  C++ and Python
u(i)=20.+(i-1)*rank Fortran (just so the answer is the same
```

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_send_array.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_send_array.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpi_send_array.py" lang="python" >}}
{{< /spoiler >}}


