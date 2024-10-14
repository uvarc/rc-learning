program sendrows
   use mpi_f08

   double precision, allocatable, dimension(:,:)  :: u,w
   integer            :: N
   integer            :: i,j

   integer            :: nr, nc
   integer            :: rank, nprocs, tag=0
   integer            :: err, errcode
   integer            :: ncount, blocklength, stride
   type(MPI_Status),  dimension(:), allocatable  :: mpi_status_arr
   type(MPI_Request), dimension(:), allocatable  :: mpi_requests
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
   allocate(mpi_requests(2*nprocs),mpi_status_arr(2*nprocs))
   u=0.0d0
   w=0.0d0

   !Cyclic sending
   if (rank == nprocs-1) then
       src=rank-1
       dest=0
   else if (rank==0) then
       src=nprocs-1
       dest=rank+1
   else
       src=rank-1
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
           print *, i,i+1,i+nprocs+1
           if (i==0) then
               call MPI_Irecv(w(nprocs,1),1,rows,src,tag,MPI_COMM_WORLD,mpi_requests(i+1))
               call MPI_Isend(u(i+1,1),1,rows,dest,tag,MPI_COMM_WORLD,mpi_requests(i+nprocs+1))
            else if (i==nprocs-1) then
               call MPI_Irecv(w(1,1),1,rows,src,tag,MPI_COMM_WORLD,mpi_requests(i+1))
               call MPI_Isend(u(nprocs,1),1,rows,dest,tag,MPI_COMM_WORLD,mpi_requests(i+nprocs+1))
            else
               call MPI_Irecv(w(i+2,1),1,rows,src,tag,MPI_COMM_WORLD,mpi_requests(i+1))
               call MPI_Isend(u(i+1,1),1,rows,dest,tag,MPI_COMM_WORLD,mpi_requests(i+nprocs+1))
            endif
        endif
   enddo

   call MPI_Waitall(size(mpi_requests),mpi_requests,mpi_status_arr)


   call MPI_TYPE_FREE(rows)

   !Print neatly
   do i=1,nr
      write(*,*) "|",u(i,:),"|","    |",w(i,:),"|"
   enddo

   call MPI_Finalize()

end program




