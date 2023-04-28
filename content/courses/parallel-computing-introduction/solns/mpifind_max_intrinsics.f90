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
   real    :: zval, zmax
   real, dimension(:),   allocatable :: xvals, yvals
   real, dimension(:,:), allocatable :: zvals
   integer :: i, j, nargs
   integer :: nsamps
   integer :: max_xind, max_yind
   real    :: xmax, ymax
   character(len=12) :: n

   integer :: rank, nprocs, ierr
   integer, dimension(:), allocatable:: seed
   integer               :: iseed, icount
   integer, dimension(2) :: maxinds
   real,    dimension(:), allocatable:: zresult

   interface
      real function surface(x,y)
         implicit none
         real, intent(in) :: x, y
      end function
   end interface

   nargs=command_argument_count()
   if ( nargs .ne. 1 ) then
      stop "Usage: arguments are lower bound, upper bound, number of steps"
   else
      call get_command_argument(1,n)
      read(n,'(i12)') nsamps
   endif

   call MPI_Init(ierr)
   call MPI_Comm_rank(MPI_COMM_WORLD,rank,ierr)
   call MPI_Comm_size(MPI_COMM_WORLD,nprocs,ierr)

   ! Define the parameters to test
   xlo=-10.*pi; xhi=10.*pi
   ylo=-10.*pi; yhi=10.*pi

   allocate(xvals(nsamps),yvals(nsamps))
   allocate(zresult(nprocs))

!For correct statistical properties, we set random seeds for each rank
   call system_clock(icount)
   iseed=icount+rank
   call set_random_seed(iseed)

!With the distribution of points we can use faster intrinsics
   allocate(zvals(nsamps,nsamps))
   max_xind=1
   max_yind=1
   zmax=0.
   do i=1,nsamps
     do j=1,nsamps
        xvals(i)=urand(xlo,xhi)
        yvals(i)=urand(ylo,yhi)
        zvals(i,j)=surface(xvals(i),yvals(j))
     enddo
   enddo

   zmax=maxval(zvals)
   print *, zmax

   do i=1,nsamps
       do j=1,nsamps
           if (zvals(i,j)==zmax) then
              xmax=xvals(i)
              ymax=yvals(j)
              go to 10
           endif
       enddo
   enddo

10 continue
   print *, xmax,ymax

   call MPI_Gather(zmax,1,MPI_REAL,zresult,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)

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

