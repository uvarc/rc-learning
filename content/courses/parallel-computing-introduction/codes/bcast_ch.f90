program broadcaster
    use mpi
    implicit none

    integer           :: nprocs, rank, ierr
    integer           :: sendcount
    character(len=15) :: message

    call MPI_Init(ierr)
    call MPI_Comm_size(MPI_COMM_WORLD,nprocs,ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD,rank,ierr)

    if (rank==0) then
        message="I have a secret"
    else
        message='               '
    endif

    sendcount=len(message)

    call MPI_Bcast(message,sendcount,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)

    print*, rank, message

    call MPI_Finalize(ierr)

end program 
