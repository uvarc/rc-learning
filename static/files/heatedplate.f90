program heatedplate
  implicit none

  integer, parameter:: maxiter=100000
  integer           :: numargs, i, j, iterations = 0
  double precision  :: eps, mean, diff = 1.0
  character(len=80) :: arg, filename
! Fixed N for speed comparison, would use allocatable in real code
  integer, parameter:: N=500
  double precision, dimension(0:N+1,0:N+1)  :: u, w
  double precision  :: topBC, bottomBC, edgeBC
  double precision  :: time0, time1

  ! check number of parameters and read in epsilon
  numargs=command_argument_count()
  if (numargs .ne. 2) then
     stop 'USAGE: <epsilon> <output-file>'
  else
     call get_command_argument(1,arg)
     read(arg,*) eps
     call get_command_argument(2,filename)
  endif
  write (*,*) 'running until the difference is <= ', eps

  ! Set boundary values and compute mean boundary value. 
  ! This has the ice bath on the top edge.

  topBC=100.d0
  bottomBC=0.d0
  edgeBC=100.d0

  u=0.d0
  u(1:N,0)=edgeBC
  u(1:N,N+1)=edgeBC
  u(N+1,1:N)=topBC

  ! Initialize interior values to the boundary mean
  mean=sum(u(:,0))+sum(u(:,N+1))+sum(u(N+1,:))+sum(u(0,:))
  mean = mean / (4.0 * (N+2))

  u(1:N,1:N)=mean

  diff=eps

  ! Compute steady-state solution
  do while ( diff >= eps )
     diff=0.d0
     do j=1,N
        do i=1,N
           w(i,j) = (u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1))/4.0
           diff=max(diff,abs(w(i,j)-u(i,j)))
        enddo
     enddo

     if (diff >= eps) then
        u(1:N,1:N)=w(1:N,1:N)
     endif
     iterations = iterations + 1
  enddo
  write (*,*) 'completed; iterations = ', iterations

  ! Write solution to output file
  open (unit=10,file=filename)
  write (10,*) N, N
  do i=1,N
     write (10,*) u(i,1:N)
  enddo

  ! All done!
  write (*,*) "wrote output file ", filename

end program heatedplate
