---
title: "MPI IO Fileviews"
toc: true
type: book
weight: 550
date: "2026-07-14T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

Any MPI file, like all files, is a linear sequence of bytes. We could store any data in any order we wished by careful computation of offsets.  However, this rapidly becomes complicated and error-prone for more complex data structures.  Fortunately, MPI provides the _fileview_ to help with this task.  The fileview is the "window" into the file that each process sees. In this way, we can define the global structure without having to allocate space for it on any process.  

One of the most common data structures for numerically-intensive programming is the multidimensional array.  We have used them for many examples. To create a fileview, we assign a portion of the array to each process, generally by the usual topology computation.

{{< figure src="/courses/parallel-computing-introduction/img/mpi_fileview.png" atl="Illustration of a global array with parts assigned to different processes." caption="Assignment of a portion of a global array to each process." >}}

To accomplish this, we use a [subarray](/courses/parallel-computing-intro/distributed_mpi_subarray).  When we examined subarrays previously, we created subarrays as subsets of the local array on each process.  For a fileview, we will create a subarray relative to the global array.  We use the topology to compute the start and stop positions for row and column (Fortran programmers, remember we count from 0). The global dimensions make up the size and the local dimensions are the subsizes. Remember that the global array does not need to exist on the process; we are telling MPI how to subdivide it.    

After defining the subarray we pass it to `MPI_File_set_view`, with the file handle of a previously-opened MPI file. We can then call `MPI_File_write<_all>` or `MPI_file_read<_all>` as we have done before, passing it the local data array.

```c++
MPI_File_set_view(fh, offset, elem_type, subtype, repr, info);
MPI_File_write_all(fh, loc_dat, count , elem_type, status);
```

```fortran
call MPI_FILE_SET_VIEW(fh, offset, elem_type, subtype, repr, info)
call MPI_FILE_WRITE_ALL(fh,loc_data, count, elem_type, status)
```
```
```python
fh.Set_view(offset, elem_type, subtype, repr, info)
fh.Write_all(local_dat)
```

In this syntax, `elem_type` is the elementary type and `subtype` is the subarray we defined.

The `repr` is a string indicating the data representation format. It can take values:

<<{ table >}}
|  Value |  Representation |
|--------|-----------------|
| 'native' | bytes are dumped directly from memory. |
| 'internal' | the representation used internally by the specific MPI implementation. |
| 'external`| The XDR portable data formats are used. |

XDR (External Data Representation) is a standard intended to make data easily convertible from one representation to another. It was especially important before computer systems largely settled on one standard; however, data representation can still vary based on platform and XDR is still used in a variety of applications, including network transmissions.

MPI guarantees full interoperability within an environment, so `native` is often used for the data representation if the file will be written and read on the same platform.  Otherwise the representation `external32` for 32-bit XDR data is widely used. 

A full example for each language is below:

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_io_fileview.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_io_fileview.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_io_fileview.py" lang="python" >}}
{{< /spoiler >}}

