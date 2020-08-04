program hello 
use mpi 
implicit none
integer ::  myrank, nprocs 
integer ::  err 
call MPI_INIT(err) 
call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, err) 
call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, err) 
if ( myrank .eq. 0 ) then 
   print *, 'Running on ',nprocs,' Processes' 
endif 
print *, 'Greetings from process ', myrank
flush(6)
call MPI_Finalize(err)
end program