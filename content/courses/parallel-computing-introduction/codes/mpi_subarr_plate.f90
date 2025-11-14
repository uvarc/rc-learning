program sendrows
   use mpi_f08

   integer, parameter :: maxiter=10000000
   integer            :: N, M
   integer            :: i,j
   integer            :: numargs, diff_interval, interations=0
   double precision   :: eps, diff = 1.0
   character(len=80)  :: arg, filename
   character(len=4)   :: char_row, char_col
   double precision, allocatable, dimension(:,:)  :: u, w
   double precision, allocatable, dimension(:,:)  :: sbuf_right,sbuf_left
   double precision, allocatable, dimension(:,:)  :: rbuf_right,recvleft
   double precision   :: time0, time1

   double precision   :: rows
   integer            :: nrows, ncols, nrl, ncl
   integer            :: rank, nprocs, tag=0
   integer            :: left, right, up, down
   integer            :: nghosts, w_halo
   integer, dimension(2) :: dims, grid_coords
   logical, dimension(2) :: periods
   logical            :: reorder
   integer            :: ndims, grid_rank
   integer            :: direction, displ
   integer, parameter :: root=0
   type(MPI_Comm)     :: grid_comm
   type(MPI_Status)   :: mpi_stat
   type(MPI_Datatype) :: sbuf_up, sbuf_down, rbuf_up, rbuf_down
   double precision   :: gdiff
   character(len=24)  :: fname

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
   call MPI_COMM_RANK(MPI_COMM_WORLD,rank)

   ! check number of parameters and read in epsilon, filename, optionally size
   ! all ranks do this
   numargs=command_argument_count()
   if (numargs .lt. 2) then
      call MPI_Finalize()
      stop 'USAGE: epsilon output-file <N> <M>'
   else
      call get_command_argument(1,arg)
      read(arg,*) eps
      call get_command_argument(2,filename)

      if (numargs==2) then
         N=500
         M=500
      endif
      if (numargs==3) then
         call get_command_argument(3,arg)
         read(arg,*) N
         M=N
      endif
      if (numargs==4) then
         call get_command_argument(3,arg)
         read(arg,*) N
         call get_command_argument(4,arg)
         read(arg,*) M
      endif
   endif

   !For simplicity, we will limit this code to perfect square process count
   rows=sqrt(dble(nprocs))
   if ( rows /= dble(int(rows)) ) then
       call MPI_Finalize()
       stop "This code requires a perfect square number of processes"
   else
       nrows=int(rows)
       ncols=nrows
   endif

   !Strong scaling
   !Weak scaling would set nrl=N and ncl=M
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
   periods(1) = .false.  ! cyclic in this direction
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
   nghosts=1        ! number of ghost zones
   w_halo=2*nghosts ! total halo width in each dimension (including corners)
   allocate(u(0:nrl+w_halo,0:ncl+w_halo), w(0:nrl+w_halo,0:ncl*w_halo))
   u=0.d0
   w=0.d0

   ! Set up sendrecv/buffers
   subsizes  = [nghosts,ncl]
   sizes  = [nrl+2*nghosts,ncl+2*nghosts]

   ! Must use C origins
   starts = [nghosts,0]
   call MPI_Type_create_subarray(sizes, subsizes, starts, MPI_ORDER_FORTRAN,   &
                                 MPI_DOUBLE_PRECISION, sbuf_up)
   call MPI_TYPE_COMMIT(sbuf_up)

   starts = [nrl+nghosts,0]
   call MPI_Type_create_subarray(sizes, subsizes, starts, MPI_ORDER_FORTRAN,   &
                                 MPI_DOUBLE_PRECISION, rbuf_down)
   call MPI_TYPE_COMMIT(rbuf_down)

   starts = [nrl,0]
   call MPI_Type_create_subarray(sizes, subsizes, starts, MPI_ORDER_FORTRAN,   &
                                 MPI_DOUBLE_PRECISION, sbuf_down)
   call MPI_TYPE_COMMIT(sbuf_down)

   starts = [0,0]
   call MPI_Type_create_subarray(sizes, subsizes, starts, MPI_ORDER_FORTRAN,   &
                                 MPI_DOUBLE_PRECISION, rbuf_up)
   call MPI_TYPE_COMMIT(rbuf_up)

   ! Corners handled by row exchanges
   starts = [nghosts,ncl+nghosts]
   call MPI_Type_create_subarray(sizes, subsizes, starts, MPI_ORDER_FORTRAN,   &
                                 MPI_DOUBLE_PRECISION, sbuf_left)
   call MPI_TYPE_COMMIT(sbuf_left)

   starts = [nghosts,0]
   call MPI_Type_create_subarray(sizes, subsizes, starts, MPI_ORDER_FORTRAN,   &
                                 MPI_DOUBLE_PRECISION, rbuf_right)
   call MPI_TYPE_COMMIT(rbuf_right)

   starts = [nghosts,nghosts]
   call MPI_Type_create_subarray(sizes, subsizes, starts, MPI_ORDER_FORTRAN,   &
                                 MPI_DOUBLE_PRECISION, sbuf_right)
   call MPI_TYPE_COMMIT(sbuf_right)

   starts = [nghosts,ncl+nghosts-1]
   call MPI_Type_create_subarray(sizes, subsizes, starts, MPI_ORDER_FORTRAN,   &
                                 MPI_DOUBLE_PRECISION, rbuf_left)
   call MPI_TYPE_COMMIT(rbuf_left)


   !Physical boundary conditions
   call set_bcs(nghosts,nrl,ncl,nrows,ncols,grid_coords,u)

   if ( grid_coords(1)==nrows-1) then
!         print *,grid_rank, grid_coords, u(nrl+1,:)
   endif

   diff_interval=1

  ! Walltime from process 0 is good enough for me
   if (grid_rank .eq. 0) then
      write (*,'(a,es15.8,a,i4,a,i4)') 'Running until the difference is <= ', &
                                              eps,' for global size ',N,'x',M
     time0=MPI_WTIME()
   endif

  ! Compute steady-state solution
   do while ( iterations<=maxiter )

     ! Exchange halo values
     ! Send left and right

     call MPI_SENDRECV(u(1:nrl,1)    ,nrl,MPI_DOUBLE_PRECISION,left,tag,       &
                       u(1:nrl,ncl+1),nrl,MPI_DOUBLE_PRECISION,right,tag,      &
                                         grid_comm,mpi_stat)

     call MPI_SENDRECV(u(1:nrl,ncl)  ,nrl,MPI_DOUBLE_PRECISION,right,tag,      &
                       u(1:nrl,0)    ,nrl,MPI_DOUBLE_PRECISION,left,tag,       &
                                          grid_comm,mpi_stat)

     ! Send up and down

     call MPI_SENDRECV(u(nrl,0)    ,1,sbuf_down,down,tag,                     &
                       u(0,0)  ,1,rbuf_up,up,tag,                             &
                                          grid_comm,mpi_stat)

      call MPI_SENDRECV(u(1,0)    ,1,sbuf_up, up,tag,                          &
                        u(nrl+1,0)   ,1,rbuf_down, down,tag,                   &
                                          grid_comm,mpi_stat)



       do j=1,ncl
           do i=1,nrl
               w(i,j) = 0.25*(u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1))
           enddo
       enddo

       if (mod(iterations,diff_interval)==0) then
           if (diff_interval==-1) continue  !disable convergence test
               diff=maxval(abs(w(1:nrl,1:ncl)-u(1:nrl,1:ncl)))
               call MPI_ALLREDUCE(diff,gdiff,1,MPI_DOUBLE_PRECISION,MPI_MAX,  &
                                                             MPI_COMM_WORLD)

               if (gdiff <= eps) then
                   exit
               endif
       endif

      !Set halo values
       w(0,:)=u(0,:)
       w(nrl+1,:)=u(nrl+1,:)
       w(:,0)=u(:,0)
       w(:,ncl+1)=u(:,ncl+1)

       u = w

     ! Reset physical boundaries (they get overwritten in the halo exchange)
       call set_bcs(nghosts,nrl,ncl,nrows,ncols,grid_coords,u)

      !If this happens we will exit at next conditional test.
      !Avoids explicit break here
       if (iterations>maxiter) then
          if (grid_rank==0) then
              write(*,*) "Warning: maximum iterations exceeded"
          endif
       endif

       iterations = iterations + 1

   enddo

   if (grid_rank==root) then
     time1 = MPI_WTIME()
     write(*,*) 'completed; iterations = ', iterations,"in ",time1-time0," sec"
   endif


  ! Write solution to output file by row. Must use correct stitching to get
  ! overall result, such as a numpy concatenate on axis=1.
  ! It would also work to output by columns, in which case stitching is more
  ! straightforward.

  !Some compilers (Intel) like to add EOLs to line up columns in output. The
  ! Fortran standard permits this with * IO, but it complicates plotting.
  ! Use some F08 features to fix this.

  write(char_row,'(i4)')  grid_coords(1) 
  write(char_col,'(i4)')  grid_coords(2) 
  fname=trim(filename)//trim(adjustl(char_row))//trim(adjustl(char_col))

  open (unit=10,file=fname)
  do i=1,nrl
     write (10,'(*(g0,1x))') u(i,1:ncl)
  enddo


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

  topBC=0.d0
  bottomBC=100.d0
  rightBC=100.d0
  leftBC=100.d0

  !Set boundary conditions
   if (coords(1)==0) then
      u(0:nghosts-1,:)=topBC
   else if (coords(1)==nrows-1) then
      u(nr+1:nr+nghosts:)=bottomBC
   endif
   if (coords(2)==0) then
      u(:,0:nghosts-1)=leftBC
   else if (coords(2)==ncols-1) then
      u(:,nc+1:nc+nghosts)=rightBC
   endif

end subroutine set_bcs

