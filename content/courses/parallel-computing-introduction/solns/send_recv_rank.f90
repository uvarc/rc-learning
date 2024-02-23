program send_receive
use mpi
implicit none

integer ::  rank, partner, nprocs, ierr
integer, dimension(MPI_STATUS_SIZE) :: status
integer :: half, message

   call MPI_Init(ierr)
   call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr);
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr);

    if ( mod(nprocs,2) /= 0 ) then
        write(*,*) "This program runs only on an even number of processes"
        call MPI_Finalize(ierr)
        stop
    endif

    half = nprocs/2
    if ( rank < half ) then
        partner= half+rank
    else 
        partner=rank-half
    endif

    if ( rank < nprocs/2 ) then
        call MPI_Recv(message,1,MPI_INTEGER,partner,0,MPI_COMM_WORLD,status,ierr);
        call MPI_Send(rank,1,MPI_INTEGER,partner,0,MPI_COMM_WORLD,ierr)
    else 
        call MPI_Send(rank,1,MPI_INTEGER,partner,0,MPI_COMM_WORLD,ierr)
        call MPI_Recv(message,1,MPI_INTEGER,partner,0,MPI_COMM_WORLD,status,ierr)
    endif

    write(*,*) rank, message

    call MPI_Finalize(ierr)

end program

