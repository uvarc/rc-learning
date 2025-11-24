program mpi_groups
use mpi_f08

  integer :: nprocs, rank, new_comm_rank
  integer, allocatable, dimension(:) :: evens, flag
  integer :: nevens
  integer :: ne, r
  character(len=15) :: message
  type(MPI_Group) :: world_group, new_group
  type(MPI_Comm)  :: new_comm

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank)
  call MPI_Comm_size(MPI_COMM_WORLD, nprocs)

  allocate(flag(0:nprocs-1))
  do r=0,nprocs-1
     if (mod(r,2)==0) then
        flag(r)=0
     else
        flag(r)=1
     endif
  enddo

  nevens=count(flag==0)

  allocate(evens(nevens))

  ne=1
  do r=0,nprocs-1
     if (flag(r)==0) then
        evens(ne)=r
        ne=ne+1
     endif
  enddo

  call MPI_Comm_group(MPI_COMM_WORLD, world_group)

  if (flag(rank)==0) then
      call MPI_Group_incl(world_group, nevens, evens, new_group)
  else
      call MPI_Group_excl(world_group, nevens, evens, new_group)
  endif

  call MPI_Comm_create(MPI_COMM_WORLD, new_group, new_comm)

  call MPI_Comm_rank(new_comm, new_comm_rank)

  if (new_comm_rank==0) then
      if (flag(rank)==0) then
          message="The secret is 1"
      else
          message="The secret is 2"
      endif
  endif

  call MPI_Bcast(message,len(message),MPI_CHARACTER,0,new_comm)

  print *, 'rank= ',rank,' new_rank= ',new_comm_rank, message

  call MPI_Finalize(ierr)

end program
