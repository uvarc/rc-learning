program mpi_threads
use mpi_f08

integer ::  provided

   call MPI_INIT_THREAD(MPI_THREAD_MULTIPLE, provided)
   write(*,'(a,i2,a,i2)') "Requested ",MPI_THREAD_MULTIPLE," Provided ",provided
   call MPI_Finalize()

end program
