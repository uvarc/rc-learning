program heatedplate
  use mpi
  implicit none

  integer, parameter:: maxiter=10000000
  integer           :: N, M, lb1, lb2
  integer           :: numargs, i, j, diff_interval, iterations = 0
  double precision  :: eps, mean, diff = 1.0
  character(len=80) :: arg, filename
  double precision, allocatable, dimension(:,:) :: u, w
  double precision  :: bc1, bc2, bc3, bc4
  double precision  :: time0, time1
  logical           :: strong_scaling

  !Add for MPI
  integer            :: nrl, ncl
  integer            :: rank, nprocs, tag=0
  integer            :: errcode, ierr
  integer, dimension(MPI_STATUS_SIZE) :: mpi_stat
  integer, parameter :: root=0
  integer            :: left, right
  double precision   :: gdiff
  character(len=24)  :: fname

  interface
    subroutine set_bcs(lb1,lb2,nr,nc,rank,nprocs,u)
       implicit none
       integer, intent(in)  :: lb1, lb2, nr, nc, rank, nprocs
       double precision, dimension(lb1:,lb2:), intent(inout) :: u
    end subroutine
  end interface

  !Initialize MPI, get the local number of columns
  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)

  ! check number of parameters and read in epsilon, filename, optionally size
  ! all ranks do this
  numargs=command_argument_count()
  if (numargs .lt. 2) then
     call MPI_Finalize(ierr)
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

  strong_scaling=.true.

  if (strong_scaling) then
     ! strong scaling
     if (mod(M,nprocs).ne.0) then
         call MPI_FINALIZE(ierr)
         stop 'Not an even division of the array'
      else
          ncl = M/nprocs
      endif
  else
  ! weak scaling
      ncl = M
      M = M*nprocs  !for printing size
  endif

  ! Both
  nrl=N

  ! Find my neighbors
  if ( rank .eq. root ) then
      left = MPI_PROC_NULL
  else
      left = rank - 1
  endif

  if ( rank .eq. nprocs-1 ) then
      right = MPI_PROC_NULL
  else
      right = rank + 1
  endif

  ! Set boundary values and compute mean boundary value. 
  ! This has the ice bath on the top edge.

  ! Allocate arrays)
  lb1=lbound(u,1)
  lb2=ubound(u,2)
  allocate(u(lb1:nrl+1,lb2:ncl+1),w(lb1:nrl+1,lb2:ncl+1))

  u=0.d0
  w=0.d0

  ! Set physical boundaries
  call set_bcs(lb1,lb2,nrl,ncl,rank,nprocs,u)

  diff_interval=1

  ! Walltime from process 0 is good enough for me
  if (rank .eq. 0) then
    time0=MPI_WTIME()
  endif

  if (rank==0) then
      write (*,'(a,es15.8,a,i6,a,i6)') 'Running until the difference is <= ',  &
                                              eps,' for global size ',N,'x',M
  endif

  ! Compute steady-state solution
  do while ( iterations<=maxiter )

     ! Exchange halo values
     call MPI_SENDRECV(u(1:nrl,1)    ,nrl,MPI_DOUBLE_PRECISION,left,tag,       &
                       u(1:nrl,ncl+1),nrl,MPI_DOUBLE_PRECISION,right,tag,      &
                                          MPI_COMM_WORLD,mpi_stat,ierr)

     call MPI_SENDRECV(u(1:nrl,ncl)  ,nrl,MPI_DOUBLE_PRECISION,right,tag,      &
                       u(1:nrl,0)    ,nrl,MPI_DOUBLE_PRECISION,left,tag,       &
                                          MPI_COMM_WORLD,mpi_stat,ierr)

     do j=1,ncl
        do i=1,nrl
           w(i,j) = 0.25*(u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1))
        enddo
     enddo

     !Set halo values
     w(0,:)=u(0,:)
     w(nrl+1,:)=u(nrl+1,:)
     w(:,0)=u(:,0)
     w(:,ncl+1)=u(:,ncl+1)

     if (mod(iterations,diff_interval)==0) then
           if (diff_interval==-1) continue  !disable convergence test
           diff=maxval(abs(w(1:nrl,1:ncl)-u(1:nrl,1:ncl)))
           call MPI_ALLREDUCE(diff,gdiff,1,MPI_DOUBLE_PRECISION,MPI_MAX,       &
                                                   MPI_COMM_WORLD, ierr)

           if (gdiff <= eps) then
              exit
           endif
     endif

     u = w

     ! Reset physical boundaries (they get overwritten in the halo exchange)
     call set_bcs(lb1,lb2,nrl,ncl,rank,nprocs,u)

     iterations = iterations + 1

     !If this happens we will exit at next conditional test.
     !Avoids explicit break here
     if (iterations>maxiter) then
           if (rank==0) then
               write(*,*) "Warning: maximum iterations exceeded"
           endif
     endif
  enddo

  if (rank==root) then
    time1 = MPI_WTIME()
    write (*,*) 'completed; iterations = ', iterations,"in ",time1-time0," sec"
  endif

  ! Write solution to output file by row. Must use correct stitching to get
  ! overall result, such as a numpy concatenate on axis=1.
  ! It would also work to output by columns, in which case stitching is more
  ! straightforward.

  !Some compilers (Intel) like to add EOLs to line up columns in output. The 
  ! Fortran standard permits this with * IO, but it complicates plotting.  
  ! Use some F08 features to fix this.

  write(fname,'(a,i4.4)') filename(1:len_trim(filename)),rank

  open (unit=10,file=fname)
  do i=1,nrl
     write (10,'(*(g0,1x))') u(i,1:ncl)
  enddo

  ! All done!
  call MPI_Finalize(ierr)

end program heatedplate

subroutine set_bcs(lb1,lb2,nr,nc,rank,nprocs,u)
implicit none
integer, intent(in)                     :: lb1, lb2, nr, nc, rank, nprocs
double precision, dimension(lb1:,lb2:), intent(inout) :: u
double precision                        :: topBC, bottomBC, leftBC, rightBC

  ! Set boundary values 
  ! This has the ice bath on the bottom edge.
  ! Note: when plotting, 0,0 is the bottom.

  topBC=100.d0
  bottomBC=0.d0
  leftBC=100.d0
  rightBC=100.d0

  !Set boundary conditions
  !Fortran stitches by column
   u(lb1,:)=topBC
   u(nr+1,:)=bottomBC
   if (rank==0) then
      u(1:nr,lb2)=leftBC
   else if (rank==nprocs-1) then
      u(1:nr,nc+1)=rightBC
   endif

end subroutine set_bcs
