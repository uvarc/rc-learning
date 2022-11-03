program everybodyskate
    use mpi
    implicit none

    real, dimension(:), allocatable :: myvals
    real, dimension(:), allocatable :: recvals
    integer              :: i
    integer              :: rank, nprocs, ierr

    call MPI_Init(ierr)
    call MPI_Comm_size(MPI_COMM_WORLD,nprocs,ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD,rank,ierr)

    allocate(myvals(nprocs))

    myvals(1)=(rank+1)*100.
    do i=2,nprocs
        myvals(i)=myvals(i-1)+1.
    enddo

    allocate(recvals(nprocs))
    call MPI_Alltoall(myvals,1,MPI_REAL,recvals,1,MPI_REAL,MPI_COMM_WORLD,ierr)

    call MPI_Finalize(ierr)

end program
