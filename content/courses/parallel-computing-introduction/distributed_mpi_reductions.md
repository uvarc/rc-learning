---
date: "2020-11-17"
title: "Global Communication in MPI: Reduction"
weight: 58
---

In a _reduction_, data are sent to a root process, which performs a specified binary operation on sequential pairs of data.

Ignoring details of implementation, which as described would be very inefficient, consider this example.  We want to compute the sum of all the local values of some variable over all ranks in the communicator group.  The root process, assumed to be rank 0 for our case, has a value in its buffer.  It receives another value from rank 1, adds it to the 0 value, and stores the result into a "global" buffer.  Next it receives the value from rank 2.  It adds this value to the contents of the global buffer.  The value from rank 3 is then received and added to the global buffer variable.  This continues until all ranks have sent their value to the root process.

The standard MPI Datatypes that we have already seen are used for the operations that return a single type of value.  If the buffer is an array, MPI_Reduce will perform the operation elementwise.

The built-in operations are the same for all languages.

{{< table >}}
|  operation  |  MPI_Op |
|-------------|---------|
|  bit-wise and  |  MPI_BAND |
|  bit-wise or  |  MPI_BOR |
|  bit-wise exclusive or  |  MPI_BXOR |
|  logical and  |  MPI_LAND |
|  logical or  |  MPI_LOR |
|  logical exclusive or  |  MPI_LXOR |
|  sum      |  MPI_SUM |
|  product  |  MPI_PROD |
|  maximum  |  MPI_MAX |
|  minimum  |  MPI_MIN |
|  maximum and its index   |  MPI_MAXLOC |
|  minimum and its index   |  MPI_MINLOC |
{{< /table >}}

The operations in Python mpi4py are the same, but with a period rather than an underscore, e.g. `MPI.SUM`.

MPI_MAXLOC and MPI_MINLOC require some special handling. They return both the value and the rank into the global variable.  The MPI_Datatypes for these two operations are particular to the language.  For more details and examples see the documentation at the [MPI Forum](https://www.mpi-forum.org/docs/mpi-3.1/mpi31-report/node114.htm#Node114).

### C/C++

```c
int MPI_Reduce(void *operand, void *result, int count, MPI_Datatype type, MPI_Op operator, int root, MPI_Comm comm);
```

{{< spoiler text="C++ Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/reduce.cxx" lang="cxx" >}}
{{< /spoiler >}}

The special predefined types used for MPI_MAXLOC and MPI_MINLOC in C/C++ assume a struct has been defined, with the first member's type as indicated and the second an `int`.

{{< table >}}
|  Types (value, index)  |  MPI_Datatype |
|-------------|---------|
| float, int  |  MPI_FLOAT_INT |
| double, int  |  MPI_DOUBLE_INT |
| long, int  |  MPI_LONG_INT |
| int, int  |  MPI_2INT |
| short, int  |  MPI_SHORT_INT |
| long double, int  |  MPI_LONG_DOUBLE_INT |
{{< /table >}}

### Fortran

```fortran
MPI_REDUCE(sendbuf, recvbuf, count, datatype, op, root, comm, ierr)
```

{{< spoiler text="Fortran Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/reduce.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< table >}}
For MPI_MAXLOC and MPI_MINLOC, Fortran derived types are not accommodated at this time, so an array with two elements of the same type must be used. The index (the second element) can be coerced to an integer if necessary.

|  Types (value, index)  |  MPI_Datatype |
|-------------|---------|
| real  |  MPI_2REAL |
| double precision  |  MPI_2DOUBLE_PRECISION |
| integer  |  MPI_2INTEGER |
{{< /table >}}

### Python

Note that the lower-case ‘reduce’ handles pickled objects; use the title-case ‘Reduce’ with NumPy arrays.

```python
comm.Reduce(sendarr, recvarr, operation, root=0)
```

{{< spoiler text="Python Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/reduce.py" lang="python" >}}
{{< /spoiler >}}

The special datatypes for MAXLOC and MINLOC in mpi4py are the same as for C, but with the underscore replaced by a period as usual (`MPI.FLOAT_INT`).

**Exercise**

Return to the random-walk program with MPI.  Add an appropriate reduction and print only the overall result.  

{{< spoiler text="Explanation" >}}
The purpose of parallelizing the random-walk code was to run a large number of trials in order to obtain a more accurate estimate of the final distance.  Without the reduction, we would have collected the results and averaged them.  We can use the reduction to accomplish this automatically.
{{< /spoiler >}}

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpirandom_walk_red.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpirandom_walk_red.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpirandom_walk_red.py" lang="python" >}}
{{< /spoiler >}}

