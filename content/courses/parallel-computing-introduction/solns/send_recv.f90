program send_receive
use mpi
implicit none

integer ::  rank, npes, ierr
integer, dimension(MPI_STATUS_SIZE) :: status
real    ::  baton

   call MPI_Init(ierr)
   call MPI_Comm_size(MPI_COMM_WORLD, npes, ierr);
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr);

    if ( npes /= 2 ) then
        write(*,*) "This program runs only on two processes"
        call MPI_Finalize(ierr)
        stop
    endif

    baton=0;

    if ( rank == 0 ) then
        call MPI_Recv(baton,1,MPI_REAL,1,0,MPI_COMM_WORLD,status,ierr);
        baton=baton+1.
        call MPI_Send(baton,1,MPI_REAL,1,0,MPI_COMM_WORLD,ierr)
    else if ( rank == 1 ) then
        baton=12.
        call MPI_Send(baton,1,MPI_REAL,0,0,MPI_COMM_WORLD,ierr)
        call MPI_Recv(baton,1,MPI_REAL,0,0,MPI_COMM_WORLD,status,ierr)
    endif

    write(*,*) rank,baton

    call MPI_Finalize(ierr)

end program

