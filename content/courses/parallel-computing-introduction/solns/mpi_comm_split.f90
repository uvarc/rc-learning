program mpi_split
use mpi_f08

  integer :: nprocs, rank, new_comm_rank
  integer ::  color, key
  character(len=15) :: message
  type(MPI_Comm)  :: new_comm

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank)
  call MPI_Comm_size(MPI_COMM_WORLD, nprocs)

  if (mod(rank,2)==0) then
     color=0
  else
     color=1
  endif

  key=0
  call MPI_Comm_split(MPI_COMM_WORLD, color, key, new_comm)

  call MPI_Comm_rank(new_comm, new_comm_rank)

  message="               "
  if (new_comm_rank==0) then
      if (mod(rank,2)==0) then
          message="The secret is 1"
      else
          message="The secret is 2"
      endif
  endif

  call MPI_Bcast(message,len(message),MPI_CHARACTER,0,new_comm)

  print *, 'rank= ',rank,' new_rank= ',new_comm_rank, message

  call MPI_Finalize(ierr)

end program
