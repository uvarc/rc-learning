program exchange
   use mpi
   implicit none

   double precision, allocatable, dimension(:) :: u, w
   integer :: nelems
   integer :: i
    
   integer :: rank, nprocs, neighbor, ierr
   integer :: sendtag, recvtag
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

   nelems=10
   allocate(u(nelems),w(nelems))
   u=0.
   w=0.

   do i=1,nelems
      !Adjust for 1 base only so Fortran gets same answer as C++ and Python
      u(i)=20.+(i-1)*rank
   enddo

   if ( mod(rank,2)==0 ) then
       neighbor = rank+1
       sendtag=1
       recvtag=2
   else
       neighbor = rank-1
       sendtag=2
       recvtag=1
   end if

   if ( mod(rank,2)==0 ) then
       call MPI_Sendrecv(u,nelems,MPI_DOUBLE_PRECISION,neighbor,sendtag,       &
                         w,nelems,MPI_DOUBLE_PRECISION,neighbor,recvtag,MPI_COMM_WORLD, &
                                                                  status,ierr)
   else
       call MPI_Sendrecv(u,nelems,MPI_DOUBLE_PRECISION,neighbor,sendtag,       &
                         w,nelems,MPI_DOUBLE_PRECISION,neighbor,recvtag,MPI_COMM_WORLD, &
                                                                  status,ierr)
   endif

   do i=1,nelems
       write(*,'(i5,i8,2f12.4)') rank, i, u(i), w(i)
   enddo

   call MPI_Finalize(ierr)

end program

