program broadcaster
    use mpi
    implicit none

    integer           :: nprocs, rank, ierr
    double precision  :: message

    call MPI_Init(ierr)
    call MPI_Comm_size(MPI_COMM_WORLD,nprocs,ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD,rank,ierr)

    if (rank==0) then
        message=42.
    endif

    call MPI_Bcast(message,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

    print*, rank, message

    call MPI_Finalize(ierr)

end program 
