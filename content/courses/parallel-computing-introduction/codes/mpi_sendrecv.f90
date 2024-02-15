program exchange
   use mpi
   implicit none
    
   integer :: rank, nprocs, neighbor, ierr
   integer :: status(MPI_STATUS_SIZE)
   integer :: message

   call MPI_Init(ierr)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
   call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)

   if (nprocs < 2) then
        write(6,*) "This program works only for at least two processes"
        call MPI_Finalize(ierr)
        stop
   else if ( mod(nprocs,2) /= 0 ) then
        write(6,*) "This program works only for an even number of processes"
        call MPI_Finalize(ierr)
        stop
   end if

   if ( mod(rank,2)==0 ) then
       neighbor = rank+1
   else
       neighbor = rank-1
   end if

   call MPI_Sendrecv(rank,1,MPI_INTEGER,neighbor,0,message,1,MPI_INTEGER,      &
                                         neighbor,0,MPI_COMM_WORLD,status,ierr)

   write(*,*) rank, message

   call MPI_Finalize(ierr)

end program

