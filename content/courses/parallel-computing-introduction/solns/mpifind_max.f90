module random
  implicit none

  ! Author:  Katherine Holcomb
  ! Shortened version of a longer module
  ! Module incorporated into this file for convenience; typically it would be in its
  ! own file.
  ! Comment out one or the other when the module is incorporated into a code.

  ! Single precision
  integer, parameter   :: rk = kind(1.0)
  ! Double precision
  !integer, parameter   :: rk = kind(1.0d0)

  contains

     subroutine set_random_seed(seed)
       ! Sets all elements of the seed array
       integer, optional, intent(in)       :: seed

       integer                             :: isize
       integer, dimension(:), allocatable  :: iseed
       integer, dimension(8)               :: idate
       integer                             :: icount, i

       call random_seed(size=isize)

       if ( .not. present(seed) ) then
          call system_clock(icount)
          allocate(iseed(isize),source=icount+370*[(i,i=0,isize-1)])
       else
          allocate(iseed(isize))
          iseed=seed
       endif

       call random_seed(put=iseed)

     end subroutine set_random_seed

     function urand(lb,ub,seed)
       ! Returns a uniformly-distributed random number in the range lb to ub.
       real(rk)                        :: urand
       real(rk), optional, intent(in)  :: lb,ub
       real(rk), optional, intent(in)  :: seed

       integer                         :: iseed
       real(rk)                        :: rnd
       real(rk)                        :: lower,upper

       if ( present(seed) ) then
          iseed=int(seed)
          call set_random_seed(iseed)
       endif

       if ( present(lb) ) then
          lower=lb
       else
          lower=0.0_rk
       endif

       if ( present(ub) ) then
          upper = ub
       else
          upper = 1.0_rk
       endif

       call random_number(rnd)
       urand = lower+(upper-lower)*rnd

       return
     end function urand
end module

program find_max
   use random
   use mpi
   implicit none

   real    :: pi=4.0*atan(1.0)
   real    :: xlo, xhi, ylo, yhi
   real    :: my_xlo, my_xhi, my_ylo, my_yhi
   real    :: zval, zmax
   real    :: xval, xincr, xmax
   real    :: yval, yincr, ymax
   real, dimension(:),   allocatable :: xvals, yvals
   real, dimension(:,:), allocatable :: zvals
   integer :: i, j, nargs
   integer :: nsamps
   character(len=12) :: n

   integer :: rank, nprocs, ierr
   integer, dimension(:), allocatable:: seed
   integer               :: iseed, icount
   integer, dimension(:), allocatable:: maxinds
   real,    dimension(:), allocatable:: xresult,yresult,zresult
   real                  :: g_zmax
   logical               :: shutdown=.false.

   interface
      real function surface(x,y)
         implicit none
         real, intent(in) :: x, y
      end function
   end interface

   call MPI_Init(ierr)
   call MPI_Comm_rank(MPI_COMM_WORLD,rank,ierr)
   call MPI_Comm_size(MPI_COMM_WORLD,nprocs,ierr)

   if (rank==0) then
      nargs=command_argument_count()
      if ( nargs .ne. 1 ) then
         shutdown=.true.
         write(*,*) "Usage: number of samples to test"
      else
         call get_command_argument(1,n)
         read(n,'(i12)') nsamps
      endif
   endif

   call MPI_Bcast(shutdown,1,MPI_LOGICAL,0,MPI_COMM_WORLD,ierr)
   if (shutdown) then
      call MPI_Finalize(ierr)
      stop
   endif

   call MPI_Bcast(nsamps,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

   ! Define the parameters to test
   xlo=-10.*pi; xhi=10.*pi
   ylo=-10.*pi; yhi=10.*pi

!For correct statistical properties, we set random seeds for each rank
   call system_clock(icount)
   iseed=icount+rank
   call set_random_seed(iseed)

   xmax=0.
   ymax=0.
   zmax=0.
   do i=1,nsamps
     do j=1,nsamps
        xval=urand(xlo,xhi)
        yval=urand(ylo,yhi)
        zval=surface(xval,yval)
        if (zval>zmax) then
            zmax=zval
            xmax=xval
            ymax=yval
        endif
     enddo
   enddo

   ! If not allgather, only root needs to allocate
   if (rank==0) then
       allocate(maxinds(nprocs))
       allocate(xresult(nprocs),yresult(nprocs),zresult(nprocs))
   endif

   call MPI_Gather(xmax,1,MPI_REAL,xresult,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)
   call MPI_Gather(ymax,1,MPI_REAL,yresult,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)
   call MPI_Gather(zmax,1,MPI_REAL,zresult,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)

   if (rank==0) then
      g_zmax=maxval(zresult)
      maxinds=maxloc(zresult)
      write(*,'(a,f0.6,a,f0.6,a,f0.6)')                                     &
           "Result is ",g_zmax," at x,y=",xresult(maxinds(1)),", ",yresult(maxinds(1))
   endif

   call MPI_Finalize(ierr)

end program

real function surface(x,y)
   implicit none
   real, intent(in) :: x, y

   real  :: pi=4.0*atan(1.0)
   real  :: mu1, mu2, sig1, sig2
   real  :: a, b
   real  :: z1, z2

   mu1=sqrt(2.0)
   mu2=sqrt(pi)
   sig1=3.1
   sig2=1.4
   z1=0.1*sin(x)*sin(x*y)
   a=(x-mu1)**2/(2*sig1**2)
   b=(y-mu2)**2/(2*sig2**2)
   z2=exp(-(a+b))/(sig1*sig2*sqrt(2.0*pi))

   surface=z1+z2
end function

