program sendrows
   use mpi_f08

   double precision, allocatable, dimension(:,:)  :: w
   double precision, allocatable, dimension(:,:)  :: u
   integer            :: N
   integer            :: i,j

   integer            :: nrows, ncols, nrl, ncl
   integer            :: rank, nprocs, tag=0
   integer, dimension(2) :: dims, coords
   logical, dimension(2) :: periods
   logical            :: reorder
   integer            :: ndims, grid_rank
   integer            :: direction, displ
   type(MPI_Comm)     :: grid_comm
   type(MPI_Status)   :: mpi_stat
   type(MPI_Datatype) :: row

   integer            :: left, right, up, down
   integer            :: uwsize

   !Initialize MPI, get the local number of columns
   call MPI_INIT()
   call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs)
   call MPI_COMM_RANK(MPI_COMM_WORLD,rank)

   N=8
   M=12

   nrows=2
   ncols=3

   if (nrows*ncols /= nprocs) then
       call MPI_Finalize()
       stop "Number of rows times columns does not equal nprocs"
   endif

   if (mod(N,nrows)==0 .and. mod(M,ncols)==0) then
       nrl = N/nrows
       ncl = M/ncols
   else
       call MPI_Finalize()
       stop "Number of ranks should divide the number of rows evenly."
   endif

   !**create cartesian topology for processes
   ndims=2
   dims(1) = nrows      ! number of rows
   dims(2) = ncols      ! number of columns
   periods(1) = .true.  ! cyclic in this direction
   periods(2) = .false. ! not cyclic in this direction
   reorder    = .false.  ! allow MPI to reorder if it sees fit
   call MPI_Cart_create(MPI_COMM_WORLD, ndims, dims, periods,                 &
                                                     reorder, grid_comm)
   call MPI_Comm_rank(grid_comm, grid_rank)
   call MPI_Cart_coords(grid_comm, grid_rank, ndims, coords)

   direction =  0   ! shift along the 1st index (0 or 1)
   displ =  1       ! shift by  1
   call MPI_Cart_shift(grid_comm, direction, displ, up, down)

   direction =  1   ! shift along the 2nd index (0 or 1)
   displ =  1       ! shift by  1
   call MPI_Cart_shift(grid_comm, direction, displ, left, right)

   !write(*,*) 'topo', grid_rank, up, down, left, right

   ! Set up values
   allocate(w(0:nrl+1, 0:ncl+1))
   w=reshape([(i,i=1,(nrl+2)*(ncl+2))],(/nrl+2,ncl+2/))
   w=(grid_rank+1)*w

   ! Boundary conditions
   topBC=0.d0
   bottomBC=200.d0
   edgeBC=100.d0
   if (coords(1)==0) then
      w(0,:)=topBC
   else if (coords(1)==nrows-1) then
      w(nrl+1,:)=bottomBC
   endif
   if (coords(2)==0) then
       w(:,0)=edgeBC
   else if (coords(2)==ncols-1) then
      w(:,ncl+1)=edgeBC
   endif

   call MPI_Type_vector(ncl+2,1,nrl+2,MPI_DOUBLE_PRECISION,row)
   call MPI_TYPE_COMMIT(row)

  ! Exchange halo values

  ! Send left and right

   call MPI_SENDRECV(w(1:nrl,1)    ,nrl,MPI_DOUBLE_PRECISION,left,tag,       &
                     w(1:nrl,ncl+1),nrl,MPI_DOUBLE_PRECISION,right,tag,      &
                                                    grid_comm,mpi_stat)
   call MPI_SENDRECV(w(1:nrl,ncl)  ,nrl,MPI_DOUBLE_PRECISION,right,tag,      &
                     w(1:nrl,0)    ,nrl,MPI_DOUBLE_PRECISION,left,tag,       &
                                                    grid_comm,mpi_stat)

  ! Send up and down

   call MPI_SENDRECV(w(1,0)  ,1, row, up,tag, w(nrl+1,0),1, row,down,tag,    &
                                                        grid_comm,mpi_stat)
   call MPI_SENDRECV(w(nrl,0)  ,1,row, down,tag, w(0,0)   ,1,row, up,tag,    &
                                                        grid_comm,mpi_stat)


! Check results 
   allocate(u(0:nrl+1,0:ncl+1))
   uwsize=(nrl+2)*(ncl+2)
   !Check columns
   u=0.d0
   if (rank==0) then
       call MPI_Recv(u,uwsize,MPI_DOUBLE_PRECISION,1,1,grid_comm,mpi_stat)
       write(*,*) "Ranks 0 and 1 check columns "

       do i=0,nrl+1
             write(*,'(*(f8.1))',advance='no') w(i,:)
             write(*,'(a)',advance='no') "|"
             write(*,'(*(f8.1))') u(i,:)
       enddo
       write(*,*)
   endif
   if (rank==1) then
       call MPI_Send(w,uwsize,MPI_DOUBLE_PRECISION,0,1,grid_comm)
   endif

   call MPI_Barrier(grid_comm)

   u=0.d0
   if (rank==1) then
       call MPI_Recv(u,uwsize,MPI_DOUBLE_PRECISION,2,2,grid_comm,mpi_stat)
       write(*,*) "Ranks 1 and 2 check columns "

       do i=0,nrl+1
             write(*,'(*(f8.1))',advance='no') w(i,:)
             write(*,'(a)',advance='no') "|"
             write(*,'(*(f8.1))') u(i,:)
       enddo
       write(*,*)
   endif

   if (rank==2) then
       call MPI_Send(w,uwsize,MPI_DOUBLE_PRECISION,1,2,grid_comm)
   endif


   call MPI_Barrier(grid_comm)

   ! Check rows including periodic exchange
   u=0.d0
   if (rank==0) then
       call MPI_Recv(u,uwsize,MPI_DOUBLE_PRECISION,3,3,grid_comm,mpi_stat)
       write(*,*) "Ranks 0 and 3 check rows including periodic exchange "

       do i=0,nrl+1
             write(*,'(*(f8.1))') w(i,:)
       enddo

       write(*,*) "--------------------------------------------------"

       do i=0,nrl+1
             write(*,'(*(f8.1))') u(i,:)
       enddo

   endif
   if (rank==3) then
       call MPI_Send(w,uwsize,MPI_DOUBLE_PRECISION,0,3,grid_comm)
   endif

   call MPI_Barrier(grid_comm)

   call MPI_Type_free(row)
   call MPI_Comm_free(grid_comm)

   call MPI_Finalize()

end program
