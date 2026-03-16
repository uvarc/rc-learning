program ghosts
   use mpi_f08
   implicit none

   integer            :: iterations
   integer            :: N, M
   integer            :: i,j
   integer            :: lb, ubr, ubc
   double precision, allocatable, dimension(:,:)  :: u,w

   double precision   :: rows
   integer            :: nrows, ncols, nrl, ncl
   integer            :: world_rank, nprocs, tag=0
   integer            :: left, right, up, down
   integer            :: nghosts, w_halo
   integer            :: nrl_total,ncl_total
   integer            :: nsubdims
   integer            :: uwsize
   integer, dimension(2) :: dims, grid_coords
   logical, dimension(2) :: periods
   integer, dimension(2) :: sizes, subsizes, starts
   logical            :: reorder
   integer            :: ndims, grid_rank
   integer            :: direction, displ
   integer, parameter :: root=0
   type(MPI_Comm)     :: grid_comm
   type(MPI_Status)   :: mpi_stat
   type(MPI_Datatype) :: sbuf_up, sbuf_down, rbuf_up, rbuf_down
   type(MPI_Datatype) :: sbuf_left, sbuf_right, rbuf_left, rbuf_right


   interface
     subroutine set_bcs(nghosts,nr,nc,nrows,ncols,coords,u)
        implicit none
        integer,                intent(in)  :: nghosts, nr, nc, nrows, ncols
        integer, dimension(:),  intent(in)                :: coords
        double precision, dimension(0:,0:), intent(inout) :: u
     end subroutine
   end interface

   !Initialize MPI, get the local number of columns
   call MPI_INIT()
   call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs)
   call MPI_COMM_RANK(MPI_COMM_WORLD,world_rank)

   ! Fixed (small) size for testing
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

   !create cartesian topology for processes
   ndims=2
   dims(1) = nrows      ! number of rows
   dims(2) = ncols      ! number of columns
   periods(1) = .true.  ! cyclic in this direction
   periods(2) = .false. ! not cyclic in this direction
   reorder    = .false.  ! allow MPI to reorder if it sees fit
   call MPI_Cart_create(MPI_COMM_WORLD, ndims, dims, periods,                 &
                                                     reorder, grid_comm)
   call MPI_Comm_rank(grid_comm, grid_rank)
   call MPI_Cart_coords(grid_comm, grid_rank, ndims, grid_coords)

   direction =  0   ! shift along the 1st index (0 or 1)
   displ =  1       ! shift by  1
   call MPI_Cart_shift(grid_comm, direction, displ, up, down)

   direction =  1   ! shift along the 2nd index (0 or 1)
   call MPI_Cart_shift(grid_comm, direction, displ, left, right)

   ! Set up values
   nghosts=2        ! number of ghost zones
   w_halo=2*nghosts ! total halo width in each dimension (including corners)
   nrl_total=nrl+w_halo
   ncl_total=ncl+w_halo

   !Assume lower bound same for both dimensions
   lb=0
   ubr=nrl_total-1
   ubc=ncl_total-1
   allocate(u(lb:ubr,lb:ubc), w(lb:ubr,lb:ubc))

   uwsize=size(w)

   w=reshape([(i,i=1,(nrl_total)*(ncl_total))],(/nrl_total,ncl_total/))
   w=(grid_rank+1)*w

   !Physical boundary conditions
   call set_bcs(nghosts,nrl,ncl,nrows,ncols,grid_coords,w)

   ! Set up sendrecv/buffers
   nsubdims  = 2
   sizes  = [nrl_total,ncl_total]

   !Rows
   subsizes  = [nghosts,ncl_total]

   ! Start indices must begin at 0 like C regardless of declared lower bound

   starts = [nghosts,0]
   call MPI_Type_create_subarray(nsubdims, sizes, subsizes, starts,            &
                             MPI_ORDER_FORTRAN,MPI_DOUBLE_PRECISION, sbuf_up)
   call MPI_TYPE_COMMIT(sbuf_up)

   starts = [nrl+nghosts,0]
   call MPI_Type_create_subarray(nsubdims, sizes, subsizes, starts,            &
                              MPI_ORDER_FORTRAN,MPI_DOUBLE_PRECISION,rbuf_down)
   call MPI_TYPE_COMMIT(rbuf_down)

   starts = [nrl,0]
   call MPI_Type_create_subarray(nsubdims, sizes, subsizes, starts,            &
                              MPI_ORDER_FORTRAN,MPI_DOUBLE_PRECISION,sbuf_down)
   call MPI_TYPE_COMMIT(sbuf_down)

   starts = [0,0]
   call MPI_Type_create_subarray(nsubdims, sizes, subsizes, starts,            &
                              MPI_ORDER_FORTRAN,MPI_DOUBLE_PRECISION, rbuf_up)
   call MPI_TYPE_COMMIT(rbuf_up)

   !Columns
   subsizes  = [nrl,nghosts]

   starts = [nghosts,ncl]
   call MPI_Type_create_subarray(nsubdims, sizes, subsizes, starts,            &
                              MPI_ORDER_FORTRAN,MPI_DOUBLE_PRECISION,sbuf_right)
   call MPI_TYPE_COMMIT(sbuf_right)

   starts = [nghosts,0]
   call MPI_Type_create_subarray(nsubdims, sizes, subsizes, starts,            &
                              MPI_ORDER_FORTRAN,MPI_DOUBLE_PRECISION,rbuf_left)
   call MPI_TYPE_COMMIT(rbuf_left)

   starts = [nghosts,nghosts]
   call MPI_Type_create_subarray(nsubdims, sizes, subsizes, starts,            &
                              MPI_ORDER_FORTRAN,MPI_DOUBLE_PRECISION,sbuf_left)
   call MPI_TYPE_COMMIT(sbuf_left)

   starts = [nghosts,ncl+nghosts]
   call MPI_Type_create_subarray(nsubdims, sizes, subsizes, starts,            &
                              MPI_ORDER_FORTRAN,MPI_DOUBLE_PRECISION,rbuf_right)
   call MPI_TYPE_COMMIT(rbuf_right)

   write(*,*) "Topology ",grid_rank, left, right, up, down

! Exchange halo values
! Send left and right


  call MPI_SENDRECV(w, 1, sbuf_left, left, tag, w, 1, rbuf_right, right, tag,  &
                                                           grid_comm,mpi_stat)

  call MPI_SENDRECV(w, 1, sbuf_right, right, tag ,w, 1, rbuf_left, left, tag,  &
                                                           grid_comm,mpi_stat)

! Send up and down

  call MPI_SENDRECV(w, 1, sbuf_down, down, tag, w, 1, rbuf_up, up, tag,        &
                                                           grid_comm,mpi_stat)

  call MPI_SENDRECV(w, 1, sbuf_up, up, tag, w, 1, rbuf_down, down, tag,        &
                                                           grid_comm,mpi_stat)

! Check results 

   !Check columns
   u=0.d0
   if (grid_rank==0) then
       call MPI_Recv(u,uwsize,MPI_DOUBLE_PRECISION,1,1,grid_comm,mpi_stat)
       write(*,*) "Ranks 0 and 1 check columns "

       do i=lb,ubr
             write(*,'(*(f6.1))',advance='no') w(i,:)
             write(*,'(a)',advance='no') "|"
             write(*,'(*(f6.1))') u(i,:)
       enddo
       write(*,*)
   endif
   if (grid_rank==1) then
       call MPI_Send(w,uwsize,MPI_DOUBLE_PRECISION,0,1,grid_comm)
   endif

   call MPI_Barrier(grid_comm)

  ! Check rows 
   u=0.d0
   if (grid_rank==0) then
       call MPI_Recv(u,uwsize,MPI_DOUBLE_PRECISION,3,3,grid_comm,mpi_stat)
       write(*,*) "Ranks 0 and 3 check rows" 

       do i=lb,ubr
             write(*,'(*(f6.1))') w(i,:)
       enddo

       write(*,*) "--------------------------------------------------"

       do i=lb,ubr
             write(*,'(*(f6.1))') u(i,:)
       enddo

   endif
   if (grid_rank==3) then
       call MPI_Send(w,uwsize,MPI_DOUBLE_PRECISION,0,3,grid_comm)
   endif

   call MPI_Barrier(grid_comm)


  call MPI_Comm_free(grid_comm)

  call MPI_Finalize()

end program


subroutine set_bcs(nghosts,nr,nc,nrows,ncols,coords,u)
  implicit none
  integer, intent(in)                      :: nghosts, nr, nc, nrows, ncols
  integer, dimension(:), intent(in)        :: coords
  double precision, dimension(0:,0:), intent(inout) :: u
  double precision                         :: topBC, bottomBC, leftBC, rightBC

  ! Set boundary values
  ! This has the ice bath on the bottom edge.
  ! Note: when plotting, 0,0 is the bottom.

  topBC=100.d0
  bottomBC=400.d0
  rightBC=350.d0
  leftBC=350.d0

  !Set boundary conditions
   if (coords(1)==0) then
      u(0:nghosts-1,:)=topBC
   else if (coords(1)==nrows-1) then
      u(nr+nghosts:,:)=bottomBC
   endif
   if (coords(2)==0) then
      u(:,0:nghosts-1)=leftBC
   else if (coords(2)==ncols-1) then
      u(:,nc+nghosts:)=rightBC
   endif

end subroutine set_bcs
