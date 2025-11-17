program sendrows
   use mpi_f08

   double precision, allocatable, dimension(:,:)  :: u,w
   integer            :: N
   integer            :: i,j

   integer            :: nr, nc
   integer            :: rank, nprocs, tag=0
   integer            :: ncount, blocklength, stride
   integer            :: nrequests
   type(MPI_Status),  dimension(:), allocatable  :: mpi_status_arr
   type(MPI_Request), dimension(:), allocatable  :: mpi_requests
   type(MPI_Datatype) :: row

   integer            :: src, dest

   !Initialize MPI, get the local number of columns
   call MPI_INIT()
   call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs)
   call MPI_COMM_RANK(MPI_COMM_WORLD,rank)

   N=nprocs
   nr=N
   nc=N+2

   nrequests=2

   allocate(u(nr,nc),w(nr,nc))
   allocate(mpi_requests(nrequests),mpi_status_arr(nrequests))

   u=reshape([(i,i=1,nr*nc)],(/nr,nc/))

   w=0.0d0

   !Cyclic sending
   if (rank == nprocs-1) then
       src=rank-1
       dest=0
   else if (rank==0) then
       src=nprocs-1
       dest=1
   else
       src=rank-1
       dest=rank+1
   endif

   ! These values pick a total of nc (ncount) items, one item 
   ! (blocklength) taken for each nr (stride) items

   ! The length of the row is the number of columns
   ncount=nc
   ! The number of items picked from each stride is 1
   blocklength=1
   ! The length of the column is the number of rows
   stride=nr

   call MPI_Type_vector(ncount,blocklength,stride,MPI_DOUBLE_PRECISION,row)

   call MPI_TYPE_COMMIT(row)

   if (rank==0) then
       call MPI_Irecv(w(1,1),1,row,src,tag,MPI_COMM_WORLD,mpi_requests(1))
       call MPI_Isend(u(1,1),1,row,dest,tag,MPI_COMM_WORLD,mpi_requests(2))
   else if (rank==nprocs-1) then
       call MPI_Irecv(w(nprocs,1),1,row,src,tag,MPI_COMM_WORLD,mpi_requests(1))
       call MPI_Isend(u(nprocs,1),1,row,dest,tag,MPI_COMM_WORLD,mpi_requests(2))
   else
       call MPI_Irecv(w(rank+1,1),1,row,src,tag,MPI_COMM_WORLD,mpi_requests(1))
       call MPI_Isend(u(rank+1,1),1,row,dest,tag,MPI_COMM_WORLD,mpi_requests(2))
   endif

   call MPI_Waitall(size(mpi_requests),mpi_requests,mpi_status_arr)

   call MPI_TYPE_FREE(row)

   !Print neatly

   ! U is the same for each rank in this example
   if (rank==0) then
         write(*,*) "U"
         do j=1,nr
            write(*,'(*(g12.6))') u(j,:)
         enddo
   endif
   call MPI_Barrier(MPI_COMM_WORLD)
   do i=1,nprocs
      call MPI_Barrier(MPI_COMM_WORLD)
      if (rank==i-1) then
         write(*,*) 
         write(*,*) "W for rank ",rank
         do j=1,nr
            write(*,'(*(g12.6))') w(j,:)
         enddo
      endif
   enddo

   call MPI_Type_free(row)

   call MPI_Finalize()

end program
