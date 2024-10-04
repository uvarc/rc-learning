program sendrows
   use mpi_f08

   double precision, allocatable, dimension(:,:)  :: u,w
   integer            :: N
   integer            :: i,j

   integer            :: nr, nc
   integer            :: rank, nprocs, tag=0
   integer            :: errcode
   type(MPI_Status)   :: recv_status
   integer            :: ncount, blocklength, stride
   type(MPI_Datatype) :: rows

   integer, parameter :: root=0
   integer            :: src, dest

   !Initialize MPI, get the local number of columns
   call MPI_INIT()
   call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs)
   call MPI_COMM_RANK(MPI_COMM_WORLD,rank)

   !We will make the matrix scale with number of processes for simplicity
   nr=nprocs
   nc=nprocs

   allocate(u(nr,nc),w(nr,nc))
   u=0.0d0
   w=0.0d0

   !Cyclic sending
   if (rank /= nprocs-1) then
       dest=0
   else
       dest=rank+1
   endif

   ncount=1
   blocklength=nc
   stride=nr

   call MPI_Type_vector(ncount,blocklength,stride,MPI_DOUBLE_PRECISION,rows)

   call MPI_TYPE_COMMIT(rows)

   do i=0,nprocs-1
       if (rank==i) then
           tag=i
           call MPI_Recv(w(i+2,1),1,rows,source,tag,MPI_COMM_WORLD,recv_status)
           call MPI_Send(u(i+1,1),1,rows,dest,tag,MPI_COMM_WORLD)
        endif
   enddo

   call MPI_TYPE_FREE(rows)

   !Print neatly
   do i=1,nr
      write(*,*) "|",u(i,:),"|","    |",w(i,:),"|"
   enddo

   call MPI_Finalize()

end program




