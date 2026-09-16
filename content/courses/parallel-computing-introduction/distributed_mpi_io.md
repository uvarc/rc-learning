---
title: "MPI IO"
toc: true
type: book
weight: 500
date: "2026-06-10T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

Up to this point, when generating output we have either collected all data on the root process and let it handle input/output, or we have set each process to write its portion of the data and then used a postprocessing script to stitch the pieces together into a single result.

Placing the entire IO burden on the root process has a number of disadvantages. It requires the full global dataset to be allocated on that process, which for very large output can create a memory bottleneck. It can significantly increase the communications overhead. Its main advantage is that it ensures that writes to a single file do not result in disordered output or file corruption.

Writing a separate file from each process addresses the problems of memory utilization and communications overhead, at the expense of creating a potentially large number of files and introducing the necessity of preparing some kind of postprocessing system.

MPI supports _parallel input and output_ to solve these problems. MPI can take care of creating the file and managing the access of each process.  The one possible disadvantage to MPI-IO is that it relies for efficiency on a true _parallel file system_.  A parallel file system supports data distributed across multiple networked storage nodes, so that multiple processes writing at nearly the same time do not bog down the performance, unlike conventional filesystems which serialize file access. Examples of parallel file systems include [Lustre](https://www.lustre.org/), a free and open-source system; [GPFS](https://www.ibm.com/docs/en/storage-scale), an IBM product sold with hardware as Storage Scale.  Systems such as Lustre and GPFS rely on the storage servers communicating over a high-bandwidth, low-latency network such as [Infiniband](https://www.nvidia.com/en-us/networking/products/infiniband/).  The Hadoop File System [HDFS](https://hadoop.apache.org/) is another free, open-source distributed filesystem. Unlike most of the others, it is designed for use on multiple-server systems that do not have high-performance networking infrastructure.

Parallel filesystems are a hallmark of high-performance computing clusters. Most large-scale MPI programs are run on such systems, on hundreds or even thousands of cores per run.  MPI-IO will still work on ordinary filesystems used on workstations, but should only be used for a few processes per run.

In general, MPI IO must be used to read a file written by MPI IO, since MPI files are binary data and the format is specific to a particular platform and sometimes MPI implementation.  

## MPI File Operations

MPI treats a file as a linear stream of bytes. It is up to the programmer to specify which processes read or write which bytes.

We can think of writing as similar to sending data, and reading as similar to receiving data.

Just as in serial IO, we must _open_ a file, perform an operation on it, then _close_ the file.  However, with MPI IO we must take into account how each process interacts with the file.

{{< figure src="/courses/parallel-computing-introduction/img/mpi_file_offsets.png" alt="illustration of MPI file layout" caption="Each process keeps track of its own locations in the MPI file" >}} 

### Opening and Closing

Some MPI IO operations must be collective; i.e. each process must invoke them.  Opening and closing must be collective. The communicator group is associated with the file when it is opened, and is not passed to further file operations. 

If the programmer wishes for only one process to open and close the file, the open must be invoked on the special communicator
```
MPI_COMM_SELF
```

A file must be opened with a mode (read only, write only, read/write) and a format indicating how bits are to correspond to variables.  Once opened, the file can be written or read.  All processes must then close the file.

**Syntax**
```c++
MPI_Open(comm,fname,amode,info,&fh)
```
```fortran
   call MPI_FILE_OPEN(comm,fname,amode,info,fh,mpi_err)
```
```python
fh=MPI.File.Open(comm,fname,amode,info)
```

In all languages, `comm` is the communicator, `fname` is the file name (for C++:use a `char` array), amode is the mode and is an `AND` sequence of specifications.  If the file doesn't exist the mode must include `MPI_MODE_CREATE`. The access permissions are `MPI_MODE_WRONLY` or `MPI_MODE_RDWR`.

For C++ and Python, use `|` for C/C++ or `+` or `ior` for Fortran.
```
amode=MPI_MODE_CREATE | MPI_MODE_WRONLY;
amode=ior(MPI_MODE_CREATE, MPI_MODE_WRONLY)
amode=MPI.MODE_CREATE | MPI.MODE_WRONLY
```

The possible values for amode are
{{< table >}}
| Mode  |  Purpose |
|-------|----------|
| MPI_MODE_APPEND | Open for appending. File must exist. |
| MPI_MODE_CREATE | Create the file if it does not exist.  |
| MPI_MODE_DELETE_ON_CLOSE | Delete the file when it is closed. |
| MPI_MODE_EXCL | Throw an error if the file already exists |
| MPI_MODE_RDONLY | Read only. File must exist. |
| MPI_MODE_RDWR | Read or write. |
| MPI_MODE_SEQUENTIAL | File will be accessed only sequentially. |
| MPI_MODE_WRONLY |  Write only. |
{{< /table >}}

When the operations are complete, the file is closed with MPI_File_close
```
MPI_File_close(&fh);
fh.CLose()
call MPI_FILE_CLOSE(fh)
```

Once the file has been opened, operations may be performed in individual or shared mode. In individual mode, each process maintains its own _file pointer_ to the position within the shared file.  The file pointer indicates where in the file the IO functions of that process will start.

In shared mode, all processes share the same file pointer value. We will refer the reader to MPI documentation for information about these operations.

### Writing a File

Writes can be collective or independent.  Independent writes are generally used only for lightweight operations such as reading or writing a header, or for a small amount of data per process.

The most basic write procedure is `MPI_Write`.  Each process writes whatever is specified, much like the `write` of the programming language but using MPI file formatting.

```c++
if (rank == 0)
   mpi_err=MPI_File_write(fh, buf, count, type, status);
```
```fortran
if (rank == 0) then
   call MPI_FILE_WRITE(fh, buf, count, type, status, mpi_err)
endif
```
```python
if rank==0:
   mpi_err=fh.Write([buf,type],status)
```
A test for rank is usual for an independent write. 

To be sure that a process writes to a particular location within the file, we can use `MPI_Write_at`. The programmer must specify the starting position with an offset computed for each rank.

```c++
MPI_File_write_at(fh,offset,buf,count,type,&mpi_stat);
```
```fortran
call MPI_FILE_WRITE_AT(fh, offset, buf, count, type, mpi_stat)
```
```python
fh.Write_at(offset, [buf, type],status=status)
```
The offset value is in _bytes_, not item count. The variable `offset` must be declared in C++ and Fortran as `MPI_Offset` or `INTEGER(KIND=MPI_OFFSET_KIND)` respectively.  In Python it is an integer. These declarations ensure that the integer is large enough to hold the offset, which for large files can overflow standard integers in C++ or Fortran if not properly declared.

#### Collective Writes

Individual writes are called per rank.  The MPI system does not assume that any other than the current rank is invoking the procedure. This can result in a large number of small file transactions, which are particularly inefficient on a high-performance parallel filesystem.

Collective write procedures are called by all processes. Their names are generally similar to the corresponding independent procedure, with `_all` appended.  All processes in a communicator must invoke the procedure, though it is possible and fairly common to [create communicator groups](/courses/parallel-computing-introduction) distributed_mpi_groups_comms) specifically for IO.

The communicator used for collective IO is the one passed to `MPI_File_open`. It is not explicitly passed to the procedures.

`MPI_Write_all` is the collective version of `MPI_Write`.

```c++
mpi_err=MPI_File_write_all(fh, buf, count, type, status);
```
```fortran
call MPI_FILE_WRITE_ALL(fh, buf, count, type, status, mpi_err);
```
```python
mpi_err=fh.Write_all([buf,type],status)
```

`MPI_Write_all` can require a procedure we have not discussed, `MPI_File_seek`. We can achieve similar functionality with `MPI_Write_at_all`.

```c++
MPI_File_write_at_all(fh,offset,&val,1,type,&mpi_stat);
```
```fortran
call MPI_FILE_WRITE_AT_ALL(fh, offset, val, 1, type, mpi_stat)
```
```python
fh.Write_at_all(offset, [buf, type],status=status)

These collective procedures are all _blocking_. There is a corresponding set of nonblocking procedures, but we will not discuss them here.

### Reading Files

Reading is similar to writing. The file must be opened with read permissions (`MPI_MODE_RDONLY` or `MPI_MODE_RDWR`). 

Procedures are essentially identical to their WRITE counterparts.  The most commonly used are `MPI_File_read`, `MPI_file_read_all`, `MPI_file_read_at`, and `MPI_file_read_at_all`.  As for writes, the collective calls are blocking and nonblocking versions are available.

Since reading is frequently one file for all process, so that we avoid broadcasts, we will give explicit syntax for MPI_Read_all. Others are as for the writes.

```c++
mpi_err=MPI_File_read_all(fh, buf, count, type, status);
```
```fortran
call MPI_FILE_READ_ALL(fh, buf, count, type, status, mpi_err)
```
```python
mpi_err=fh.Read_all([buf,type],status)
```

