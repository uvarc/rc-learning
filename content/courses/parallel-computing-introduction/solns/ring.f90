program ring
use mpi

implicit none

integer           :: baton

integer           :: my_rank,  npes
integer, parameter:: tag = 0
integer           :: err, errcode
integer           :: i
double precision  :: start, runtime

integer, dimension(MPI_STATUS_SIZE) :: status

call MPI_INIT(err)
call MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, err)
call MPI_COMM_SIZE(MPI_COMM_WORLD, npes, err)

if ( my_rank .eq. 0 ) then
    baton = 42
endif

if ( npes .eq. 1 ) then
   print *, 'Rank ',my_rank, 'has the baton ', baton
   call MPI_Finalize(err)
   stop
endif

if ( my_rank .eq. 0 ) then
   start = MPI_Wtime()
   call MPI_Send(baton,1,MPI_INTEGER,1,tag,MPI_COMM_WORLD,err)
   call MPI_Recv(baton,1,MPI_INTEGER,MPI_ANY_SOURCE,MPI_ANY_TAG,               &
                         MPI_COMM_WORLD,status,err)
   print *, 'Rank ',my_rank, 'has the baton ', baton
   call flush(6)
   runtime = MPI_Wtime()-start
   write(*,'(a,f0.4,a)') 'Total elapsed time was ', runtime, ' seconds'
   call flush(6)

else
     call MPI_Recv(baton,1,MPI_INTEGER,MPI_ANY_SOURCE,MPI_ANY_TAG,             &
                           MPI_COMM_WORLD,status,err)
     call MPI_Send(baton,1,MPI_INTEGER,mod(my_rank+1,npes),tag,                &
                           MPI_COMM_WORLD,err)
     write(*,*) 'Rank ',my_rank, 'has the baton ', baton
     call flush(6)

endif

call MPI_Finalize(err)

end program ring

