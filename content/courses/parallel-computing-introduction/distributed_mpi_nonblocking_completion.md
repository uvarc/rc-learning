---
title: "Completing Nonblocking Communications"
toc: true
type: docs
weight: 215
date: "2024-08-05T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

## Waiting for Requests

To complete the communications we must wait for the requests to be fulfilled.  MPI provides procedures for this.

### Wait

MPI_Wait blocks until a specific request has completed.

C++
```c++
MPI_Request request;
MPI_Status status;
MPI_Wait(&request, &status)
```
(Be sure to pay attention to how `request` and `status` are declared; they must be called by reference in any case.)


Fortran
```fortran
integer request, ierr
integer, dimension(MPI_STATUS_SIZE) :: status
! code
call MPI_Wait(request, status, ierr)
```
With the new `mpi_f08` module this becomes
```fortran
integer ierr
type(MPI_STATUS) :: status
type(MPI_Request):: request
! code
call MPI_Wait(request, status, ierr)
```

If the request is null or inactive, the procedure returns with an empty status field.

Python

In Python the methods for handling requests are invoked through the appropriate Request object.  See its [documenation](https://mpi4py.readthedocs.io/en/stable/reference/mpi4py.MPI.Request.html) for more specifics.  Note that the completion methods in mpi4py that require a list of request objects will be class methods.

```
request.Wait(status)
```
The mpi4py Wait returns a literal (`True`) when it completes. 


### Waitall

MPI_Waitall blocks until all requests have completed.

```c++
int count;
MPI_Request requests[];
MPI_Status status_arr[];
//allocate arrays somewhere to size count or declare static size
MPI_Waitall(count,requests,status_arr)
```

Fortran
```fortran
integer count, ierr
integer, dimension(:), allocatable :: requests
integer, dimension(MPI_STATUS_SIZE,*) :: status_arr
! code, allocate requests to size count somewhere
call MPI_Waitall(count,requests, status_arr, ierr)
```

Python
```python
requests=[]  #fill list to size count
status_list=list[status]
MPI.Request.Waitall(requests,status_list)
```
The mpi4py `Waitall` returns a literal `True` when it completes.

### Waitany

MPI_Waitany waits for any of an array/list of requests to complete. The `index` parameter returns the index of the request array that has completed. In C++ and Python it ranges from `0` to `count-1`.  In Fortran the range is `1` to `count`. 
C++
```c++
MPI_Request requests[];
MPI_Status status_arr[];
int index;
MPI_Waitany(count, requests, &index, status_arr)
```

Fortran
```fortran
integer count, index, ierr
integer, dimension(:), allocatable :: requests
integer, dimension(MPI_STATUS_SIZE,*) :: status_arr
! code, allocate requests to size count somewhere
call MPI_Waitany(count, requests, index, status_arry, ierr)
```

Python
```python
requests=[]  #fill list to size count
status_list=list[status]
ind=MPI.Request.Waitany(requests,status_list)
```

## Testing Requests

The `Test` family of procedures are similar to the `Wait` routines, but return a flag indicating whether the request has been completed.  `Test` routines return with a value whether the request has completed or not, so when used they are tyically contained within loops that are exited when the test is true.

### Test

C++
```c++
MPI_Request request;
MPI_Status status;
int flag;
MPI_Test(&request, &flag, MPI_Status &status)
```

Fortran
```fortran
logical :: flag
integer :: request, status(mpi_status_size), ierror
!code
call MPI_Test(request, flag, status, ierror)
```

Python
```python
flag=request.Test(status)
```

### Testall

Testall tests for each request.

```c++
int count, flag;
MPI_Request requests[];
MPI_Status status_arr[];
//allocate arrays somewhere to size count
MPI_Testall(&count,requests,&flag,status_arr)
```

Fortran
```fortran
integer :: count, ierr
logical :: flag
integer, dimension(:), allocatable :: requests
integer, dimension(MPI_STATUS_SIZE,*) :: status_arr
! code, allocate requests to size count somewhere
call MPI_Testall(count, requests, flag, status_arr, ierr)
```

Python
```python
requests=[]  #fill list to size count
status_list=list[status]
flag=MPI.Request.Testall(requests,status_list)
```

### Testany

```c++
int count, ind, flag;
MPI_Request requests[];
MPI_Status status_arr[];
//allocate arrays somewhere to size count
MPI_Testany(&count,requests,&ind,&flag,status_arr)
```

Fortran
```fortran
integer :: count, ind, ierr
logical :: flag
integer, dimension(:), allocatable :: requests
integer, dimension(MPI_STATUS_SIZE,*) :: status_arr
! code, allocate requests to size count somewhere
call MPI_Testany(count, requests, ind, flag, status_arr, ierr)
```

Python
```python
requests=[]  #fill list to size count
status_list=list[status]
ind,flag=MPI.Request.Testany(requests,status_list)
```


