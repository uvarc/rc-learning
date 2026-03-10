program ghosts
   use mpi_f08
   implicit none

   integer, parameter :: maxiters=10000000
   integer            :: iteration=0
   integer            :: N, M
   integer            :: i,j
   integer            :: numargs, diff_interval, interations=0
   integer            :: lb, ubr, ubc
   double precision, allocatable, dimension(:,:)  :: u,w

   double precision   :: eps, diff = 1.0
   character(len=80)  :: arg, filename
   character(len=4)   :: char_row, char_col

   double precision   :: time0, time1
   double precision   :: rows
   integer            :: nrows, ncols, nrl, ncl
   integer            :: world_rank, nprocs, tag=0
   integer, parameter :: root=0
   integer            :: left, right, up, down
   integer            :: nghosts, w_halo
   integer            :: nrl_gz,ncl_gz,nrl_total,ncl_total
   integer            :: nsubdims
   integer, dimension(2) :: dims, grid_coords
   logical, dimension(2) :: periods
   integer, dimension(2) :: sizes, subsizes, starts
   logical            :: reorder
   integer            :: ndims, grid_rank
   integer            :: direction, displ
   integer            :: nrequests
   type(MPI_Comm)     :: grid_comm
   type(MPI_Status)   :: mpi_stat
   type(MPI_Datatype) :: sbuf_up, sbuf_down, rbuf_up, rbuf_down
   type(MPI_Datatype) :: sbuf_left, sbuf_right, rbuf_left, rbuf_right
   type(MPI_Request), dimension(:), allocatable :: mpi_requests

   double precision   :: gdiff
   character(len=24)  :: fname


   interface
     subroutine set_bcs(lb, nghosts,nr,nc,nrows,ncols,coords,u)
        implicit none
        integer,                intent(in)  :: lb, nghosts, nr, nc, nrows, ncols
        integer, dimension(:),  intent(in)  :: coords
        double precision, dimension(0:,0:), intent(inout) :: u
     end subroutine
   end interface

   !Initialize MPI, get the local number of columns
   call MPI_INIT()
   call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs)
   call MPI_COMM_RANK(MPI_COMM_WORLD,world_rank)

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
   ! Assume number of ghosts zones same in both dimensions, lower bounds same
   nghosts=1        ! number of ghost zones
   w_halo=2*nghosts ! total halo width in each dimension (including corners)

   nrl_gz=nrl+nghosts
   ncl_gz=ncl+nghosts
   nrl_total=nrl+w_halo
   ncl_total=ncl+w_halo

   !Assume lower bound same for both dimensions
   lb=0
   ubr=nrl_total-1
   ubc=ncl_total-1
   allocate(u(lb:ubr,lb:ubc), w(lb:ubr,lb:ubc))

   u=0.d0
   w=0.d0

   !Physical boundary conditions
   call set_bcs(lb,nghosts,nrl,ncl,nrows,ncols,grid_coords,u)

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

   nrequests=8
   allocate(mpi_requests(nrequests))

   diff_interval=1

  ! Walltime from process 0 is good enough for me
   if (grid_rank .eq. 0) then
      write (*,'(a,es15.8,a,i4,a,i4)') 'Running until the difference is <= ', &
                                              eps,' for global size ',N,'x',M
     time0=MPI_WTIME()
   endif

   ! Compute steady-state solution
   do while ( iteration<=maxiters )

     ! Exchange halo values

       call MPI_Irecv(u, 1, rbuf_right, right, tag, grid_comm,mpi_requests(1))
       call MPI_Irecv(u, 1, rbuf_left,  left,  tag, grid_comm,mpi_requests(2))
       call MPI_Irecv(u, 1, rbuf_up,    up,    tag, grid_comm,mpi_requests(3))
       call MPI_Irecv(u, 1, rbuf_down,  down,  tag, grid_comm,mpi_requests(4))

       call MPI_Isend(u, 1, sbuf_left,  left,  tag, grid_comm,mpi_requests(5))
       call MPI_Isend(u, 1, sbuf_right, right, tag, grid_comm,mpi_requests(6))
       call MPI_Isend(u, 1, sbuf_down,  down,  tag, grid_comm,mpi_requests(7))
       call MPI_Isend(u, 1, sbuf_up,    up,    tag, grid_comm,mpi_requests(8))

   ! complete communications
       call MPI_Waitall(nrequests,mpi_requests,MPI_STATUSES_IGNORE)

       do j=lb+nghosts,lb+nrl_gz-1
           do i=lb+nghosts,lb+ncl_gz-1
               w(i,j) = 0.25*(u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1))
           enddo
       enddo

       if (mod(iteration,diff_interval)==0) then
           if (diff_interval==-1) continue  !disable convergence test
               diff=maxval(abs(w(nghosts:ubr-nghosts-1,nghosts:ubc-nghosts-1)  &
                   -u(nghosts:ubr-nghosts-1,nghosts:ubc-nghosts-1)))
               call MPI_ALLREDUCE(diff,gdiff,1,MPI_DOUBLE_PRECISION,MPI_MAX,   &
                                                             grid_comm)

               if (gdiff <= eps) then
                   exit
               endif
       endif

       !Update u. Don't overwrite boundaries.
       u(1:nrl,1:ncl) = w(1:nrl,1:ncl)

     ! Reset physical boundaries (they get overwritten in the halo exchange)
       call set_bcs(lb,nghosts,nrl,ncl,nrows,ncols,grid_coords,u)

      !If this happens we will exit at next conditional test.
      !Avoids explicit break here
       if (iteration>maxiters) then
          if (grid_rank==0) then
              write(*,*) "Warning: maximum iterations exceeded"
          endif
       endif

       iteration = iteration + 1

   enddo

   if (grid_rank==root) then
     time1 = MPI_WTIME()
     write(*,*) 'completed; iterations = ', iteration,"in ",time1-time0," sec"
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
   do i=lb+nghosts,lb+nghosts+nrl
      write (10,'(*(g0,1x))') u(i,1:ncl)
   enddo

   call MPI_Comm_free(grid_comm)

   call MPI_Finalize()

end program


subroutine set_bcs(lb,nghosts,nr,nc,nrows,ncols,coords,u)
  implicit none
  integer, intent(in)                      :: lb ,nghosts, nr, nc, nrows, ncols
  integer, dimension(:), intent(in)        :: coords
  double precision, dimension(lb:,lb:), intent(inout) :: u
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
      u(lb:lb+nghosts-1,:)=topBC
   else if (coords(1)==nrows-1) then
      u(nr+nghosts+lb:,:)=bottomBC
   endif
   if (coords(2)==0) then
      u(:,lb:lb+nghosts-1)=leftBC
   else if (coords(2)==ncols-1) then
      u(:,nc+nghosts+lb:)=rightBC
   endif

end subroutine set_bcs
