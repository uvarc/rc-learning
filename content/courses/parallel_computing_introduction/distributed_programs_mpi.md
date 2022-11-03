---
title: "MPI"
toc: true
type: docs
weight: 21
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
        weight: 21
---

MPI stands for  _M_ essage  _P_ assing  _I_ nterface.  It is a standard established by a committee of users and vendors.  

## Programming Languages

MPI is written in C and ships with bindings for Fortran.  Bindings have been written for many other languages, including Python and R. C\+\+ programmers should use the C functions.

Many of the examples in this lecture are C or C\+\+ code, with some Fortran and Python examples as well.  All of the C functions work the same for Fortran, with a slightly different syntax.  They are mostly the same for Python but the most widely used set of Python bindings, `mpi4py`, was modeled on the deprecated C\+\+ bindings, as they are more "Pythonic."

Guides to the most-commonly used MPI routines for the three languages this course supports can be downloaded.

[C/C++](/courses/parallel_computing_introduction/MPI_Guide_C.pdf) 

[Fortran](/courses/parallel_computing_introduction/MPI_Guide_Fortran.pdf) 

[Python](/courses/parallel_computing_introduction/MPI_Guide_mpi4py.pdf)

## Process Management

MPI programs are run under the control of an executor or _process manager_.  The process manager starts the requested number of processes on a specified list of hosts, assigns an identifier to each process, then starts the processes.  Each copy has its own global variables\, stack\, heap\, and program counter.

Usually when MPI is run the number of processes is determined and fixed for the lifetime of the program.  The MPI3 standard can spawn new processes but in a resource managed environment such as a high-performance cluster, the total number must still be requested in advance.

MPI distributions ship with a process manager called  `mpiexec`  or  `mpirun`. In some environments, such as many using Slurm, we use the Slurm process manager  `srun`.

When run outside of a resource-managed system, we must specify the number of processes through a command-line option.  If more than one host is to be used, the name of a hostlist file must be provided, or only the local host will be utilized.  The options may vary depending on the distribution of MPI but will be similar to that below:
```
mpiexec –np 16 -hosts compute1,compute2  ./myprog
```

When running with srun under Slurm the executor does  _not_  require the `-np` flag; it computes the number of processes from the resource request.  It is also aware of the hosts assigned by the scheduler.
```
srun ./myprog
```
### Message Envelopes

A _communicator_ is an object that specifies a group of processes that will communicate with one another. The default communicator is
`MPI_COMM_WORLD`. It includes all processes.  The programmer can create new communicators, usually of subsets of the processes, but this is beyond our scope.

In MPI the process ID is called the **rank**.  Rank is relative to the communicator, and is numbered from zero.  Process 0 is often called the *root process*.

A message is uniquely identified by its
- Source rank
- Destination rank
- Communicator
- Tag

The "tag" can often be set to an arbitrary value such as zero.  It is needed only in cases where there may be multiple messages from the same source to the same destination in a short time interval, or a more complete envelope is desired for some reason.

### Message Buffers

MPI documentation refers to "send buffers" and "receive buffers." These refer to  _variables_ in the program whose contents are to be sent or received.  These variables must be set up by the programmer.  The send and receive buffers cannot be the same unless the special "receive buffer" `MPI_IN_PLACE` is specified.

When a buffer is specified, the MPI library will look at the starting point in memory (the pointer to the variable).  From other information in the command, it will compute the number of bytes to be sent or received.  It will then set up a separate location in memory; this is the actual buffer. Often the buffer is not the same size as the original data since it is just used for streaming within the network.  In any case, the application programmer need not be concerned about the details of the buffers and should just regard them as _variables_.  

For the send buffer, MPI will copy the sequence of bytes into the buffer and send them over the appropriate network interface to the receiver.  The receiver will acquire the stream of data into its receive buffer and copy them into the variable specified in the program. 

### Buffer Datatypes

MPI supports most of the _primitive_ datatypes available in the target programming language, as well as a few others.

####C/C++

MPI supports most C/C++ datatypes as well as some extensions. The most commonly used are listed below.

|   C/C++ type   |  MPI_Datatype  |
|----------------|----------------|
|   int          |    MPI_INT     |
|   short        |    MPI_SHORT   |
|   long         |    MPI_LONG    |
|   long long    |    MPI_LONG_LONG_INT  |
|   unsigned int |    MPI_UNSIGNED    |
|   unsigned short |  MPI_UNSIGNED_SHORT  |
|   unsigned long |  MPI_UNSIGNED_LONG |
|   unsigned long long |  MPI_UNSIGNED_LONG_LONG |
|   float        |  MPI_FLOAT      |
|   double       |  MPI_DOUBLE     |
|   long double  |  MPI_LONG_DOUBLE     |
|   char         |  MPI_CHAR        |
|   wchar         |  MPI_WCHAR        |

Since MPI is written in C and we are not discussing the deprecated C++ bindings, the following types require headers.

|   C type       |  MPI_Datatype      |
|----------------|----------------|
|   bool         |  MPI_C_BOOL        |
|   complex         |  MPI_C_COMPLEX        |
|   double complex         |  MPI_C_DOUBLE_COMPLEX        |

Extensions

|   C/C++ type   |  MPI_Datatype  |
|----------------|----------------|
|   none         | MPI_BYTE         |
|   none         | MPI_PACKED       |

#### Fortran

|   Fortran type |  MPI_Datatype      |
|----------------|--------------------|
|   integer      |    MPI_INTEGER     |
|   real         |    MPI_REAL        |
|   double precision    |    MPI_DOUBLE_PRECISION |
|   complex      |  MPI_COMPLEX       |
|   logical      |  MPI_LOGICAL       |
|   character    |  MPI_CHARACTER     |
|   none         |  MPI_BYTE          |
|   none         |  MPI_PACKED        |

Most MPI distributions support the following types.  These are Fortran 77 style declarations; newer code should use `KIND` but care must be taken that the number of byes specified is correct.

|   Fortran type |  MPI_Datatype      |
|----------------|--------------------|
|   integer\*16      |    MPI_INTEGER16     |
|   real\*8      |    MPI_REAL8     |
|   real\*16      |    MPI_REAL16     |

#### Python

As we have mentioned, the basic MPI communication routines are in the Communicator class of the MPI subpackge of mpi4py.  Each communication subprogram has two forms, a lower-case version and another where the first letter of the method is upper case.  The lower-case version can be used to send or receive an object; mpi4py pickles it before communicating. The upper-case version works _only_ with NumPy Ndarrays.  Communicating Ndarrays is faster and is recommended when possible. However, _every_ buffer must be an Ndarray in this case, so even scalars must be placed into a one-element array.

The mpi4py package supports the C datatypes, but in the format `MPI.Dtype` rather than `MPI_Dtype`, but they are seldom required as an argument to the MPI functions.

