program broadcaster
    use mpi
    implicit none

    integer           :: nprocs, rank, ierr
    integer, dimension(:), allocatable :: values
    integer           :: i

    call MPI_Init(ierr)
    call MPI_Comm_size(MPI_COMM_WORLD,nprocs,ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD,rank,ierr)

    allocate(values(10))

    if ( rank==0 ) then
        values=[(i,i=1,10)]
    endif

    call MPI_Bcast(values,size(values),MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

    write(*,'(11i5)'), rank, values

    call MPI_Finalize(ierr)

end program 
