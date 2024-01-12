program send_receive
use mpi
implicit none

integer ::  rank, nprocs, ierr
integer, dimension(MPI_STATUS_SIZE) :: status
real    ::  message
integer ::  n

   call MPI_Init(ierr)
   call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr);
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr);

    if ( nprocs < 2 ) then
        write(*,*) "This program runs only on at least two processes"
        call MPI_Finalize(ierr)
        stop
    endif

    message=42.*rank;

    if ( rank == 0 ) then
        call MPI_Recv(message,1,MPI_REAL,1,0,MPI_COMM_WORLD,status,ierr);
    else if ( rank == nprocs-1 ) then
        call MPI_Send(message,1,MPI_REAL,nprocs-2,0,MPI_COMM_WORLD,ierr)
    else
        call MPI_Send(message,1,MPI_REAL,rank-1,0,MPI_COMM_WORLD,ierr)
        call MPI_Recv(message,1,MPI_REAL,rank+1,0,MPI_COMM_WORLD,status,ierr)
    endif

    if (rank==0) write(*,*) "First Case"
    do n=0,nprocs-1
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
            if (n==rank) then
                write(*,*) rank,42*(rank+1),message
            endif
    enddo

    call MPI_Finalize(ierr)

end program

