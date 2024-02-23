program heatedplate
  implicit none

  integer, parameter:: maxiter=10000000
  integer           :: N, M, nr, nc, lb
  integer           :: numargs, i, j, diff_interval, iterations = 0
  double precision  :: eps, mean, diff = 1.0
  character(len=80) :: arg, filename
  double precision, allocatable, dimension(:,:) :: u, w
  real              :: time0, time1

  interface
    subroutine set_bcs(lb,nr,nc,u)
       implicit none
       integer, intent(in)  :: lb, nr, nc
       double precision, dimension(lb:,lb:), intent(inout) :: u
    end subroutine
  end interface


  ! check number of parameters and read in epsilon, filename, optionally size
  numargs=command_argument_count()
  if (numargs .lt. 2) then
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

  nr=N; nc=M

  ! Set boundary values and compute mean boundary value. 
  ! This has the ice bath on the top edge.

  ! Allocate arrays)
  allocate(u(0:nr+1,0:nc+1),w(0:nr+1,0:nc+1))

  u=0.d0
  w=0.d0

  call set_bcs(lb,nr,nc,u)

  ! Initialize interior values to the boundary mean, more or les
  ! Doesn't matter much, starting from all zeroes works too. May speed
  ! up convergence a bit.
  mean=sum(u(:,0))+sum(u(:,nc+1))+sum(u(nr+1,:))+sum(u(0,:))
  mean = mean / (4.0 * (N+2))

  u(1:nr,1:nc)=mean

  !Arbitrary, just to make sure we enter loop
  diff=10.*eps

  diff_interval=1

  call cpu_time(time0)

  write (*,'(a,es15.8,a,i6,a,i6)') 'Running until the difference is <= ', eps,&
                                   ' for size ',nr,'x',nc

  ! Compute steady-state solution
  do while ( diff >= eps )
     do j=1,nc
        do i=1,nr
           w(i,j) = (u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1))/4.0
        enddo
     enddo

     if (mod(iterations,diff_interval)==0) then
           diff=maxval(abs(w(1:nr,1:nc)-u(1:nr,1:nc)))
           if (diff <= eps) then
              exit
           endif
     endif

     ! Don't overwrite BCs
     u(1:nr,1:nc)=w(1:nr,1:nc)

     if (iterations>maxiter) then
           write(*,*) "Warning: maximum iterations exceeded"
          exit
     endif
     iterations = iterations + 1
  enddo

  call cpu_time(time1)

  write (*,*) 'completed; iterations = ', iterations,"in ",time1-time0," sec"

  ! Write solution to output file by row

  !Some compilers (Intel) like to add EOls to line up columns. The Fortran
  ! standard permits this with * IO, but it complicates plotting.  Use some
  ! F08 features to fix this.
  open (unit=10,file=filename)
  do i=1,nr
     write (10,'(*(g0,1x))') u(i,1:nc)
  enddo

  ! All done!
  write (*,'(a,a)') "wrote output file ", filename

end program heatedplate

subroutine set_bcs(lb,nr,nc,u)
implicit none
integer, intent(in)                                 :: lb, nr, nc
double precision, dimension(lb:,lb:), intent(inout) :: u
double precision                                    :: topBC, bottomBC, edgeBC

  ! Set boundary values 
  ! This has the ice bath on the bottom edge.
  ! Note: when plotting, 0,0 is the bottom.

  topBC=100.d0
  bottomBC=0.d0
  edgeBC=100.d0

  !Set boundary conditions
  u(1:nr,0)=edgeBC
  u(1:nr,nc+1)=edgeBC
  u(nr+1,1:nc)=topBC

end subroutine set_bcs
