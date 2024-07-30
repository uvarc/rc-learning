---
date: "2020-11-17"
title: "Message Buffers"
weight: 23
---

MPI documentation refers to "send buffers" and "receive buffers." These refer to  _variables_ in the program whose contents are to be sent or received.  These variables must be set up by the programmer.  The send and receive buffers cannot be the same unless the special "receive buffer" `MPI_IN_PLACE` is specified.

When a buffer is specified, the MPI library will look at the starting point in memory (the pointer to the variable).  From other information in the command, it will compute the number of bytes to be sent or received.  It will then set up a separate location in memory; this is the actual buffer. Often the buffer is not the same size as the original data since it is just used for streaming within the network.  In any case, the application programmer need not be concerned about the details of the buffers and should just regard them as _variables_.  

For the send buffer, MPI will copy the sequence of bytes into the buffer and send them over the appropriate network interface to the receiver.  The receiver will acquire the stream of data into its receive buffer and copy them into the variable specified in the program. 

### Buffer Datatypes

MPI supports most of the _primitive_ datatypes available in the target programming language, as well as a few others.

In every language, it is _imperative_ that the data types in the send and receive buffers match.  If they do not, the result can be anything from garbage to a segmentation violation.

#### C/C++

MPI supports most C/C++ datatypes as well as some extensions. The most commonly used are listed below.

| C/C++ type         | MPI_Datatype           |
|--------------------|------------------------|
| int                | MPI_INT                |
| short              | MPI_SHORT              |
| long               | MPI_LONG               |
| long long          | MPI_LONG_LONG_INT      |
| unsigned int       | MPI_UNSIGNED           |
| unsigned short     | MPI_UNSIGNED_SHORT     |
| unsigned long      | MPI_UNSIGNED_LONG      |
| unsigned long long | MPI_UNSIGNED_LONG_LONG |
| float              | MPI_FLOAT              |
| double             | MPI_DOUBLE             |
| long double        | MPI_LONG_DOUBLE        |
| char               | MPI_CHAR               |
| wchar              | MPI_WCHAR              |

Specific to C:

| C type         | MPI_Datatype         |
|----------------|----------------------|
| bool           | MPI_C_BOOL           |
| complex        | MPI_C_COMPLEX        |
| double complex | MPI_C_DOUBLE_COMPLEX |

Specific to C++:

| C++ type       | MPI_Datatype           |
|----------------|------------------------|
| bool           | MPI_CXX_BOOL           |
| complex        | MPI_CXX_COMPLEX        |
| double complex | MPI_CXX_DOUBLE_COMPLEX |

Extensions

| C/C++ type | MPI_Datatype |
|------------|--------------|
| none       | MPI_BYTE     |
| none       | MPI_PACKED   |

#### Fortran

| Fortran type     | MPI_Datatype         |
|------------------|----------------------|
| integer          | MPI_INTEGER          |
| integer\*8       | MPI_INTEGER8         |
| real             | MPI_REAL             |
| double precision | MPI_DOUBLE_PRECISION |
| complex          | MPI_COMPLEX          |
| logical          | MPI_LOGICAL          |
| character        | MPI_CHARACTER        |
| none             | MPI_BYTE             |
| none             | MPI_PACKED           |

Most MPI distributions support the following types.  These are Fortran 77 style declarations; newer code should use `KIND` but care must be taken that the number of byes specified is correct.

| Fortran type | MPI_Datatype  |
|--------------|---------------|
| integer\*16  | MPI_INTEGER16 |
| real\*8      | MPI_REAL8     |
| real\*16     | MPI_REAL16    |

#### Python

As we have mentioned, the basic MPI communication routines are in the Communicator class of the MPI subpackge of mpi4py.  Each communication subprogram has two forms, a lower-case version and another where the first letter of the method is upper case.  The lower-case version can be used to send or receive an object; mpi4py pickles it before communicating.  The argument of these routines is the sent object; the received object is the return value of the function.

The upper-case version works _only_ with buffered objects, usually NumPy Ndarrays.  Communicating Ndarrays is faster and is recommended when possible. However, _every_ buffer must be a Ndarray in this case, so even scalars must be placed into a one-element array. The upper-case buffered functions are more similar to the corresponding C/C++ functions.  For the buffered functions, it is very important that the types match, so use of `dtype` is recommended in declaring NumPy arrays.

The mpi4py package supports the C datatypes, in the format `MPI.Dtype` rather than `MPI_Dtype`, but they are seldom required as an argument to the MPI functions.  It is strongly recommended that the type of each NumPy array be explicitly declared with the `dtype` option, to ensure that the types match in both send and receive buffers.  

{{< code-download file="/courses/parallel-computing-introduction/codes/mpi4py_ex.py" lang="python" >}}

