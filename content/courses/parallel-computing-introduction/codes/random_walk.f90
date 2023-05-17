module random
  implicit none

  ! Author:  Katherine Holcomb
  ! Shortened version of a longer module
  ! Module incorporated into this file for convenience; typically it would be in its 
  ! own file.
  ! Comment out one or the other when the module is incorporated into a code.

  ! Single precision
  !integer, parameter   :: rk = kind(1.0)
  ! Double precision
  integer, parameter   :: rk = kind(1.0d0)

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

     function randint(n,m)
       ! Returns a random integer between n and m.  
       integer                 :: randint
       integer, intent(in)     :: n,m

       randint=ceiling(urand(real(n-1,rk),real(m,rk)))

     end function randint

end module random
program random_walk
use random
implicit none

   integer, parameter   :: ik = selected_int_kind(15)
   integer              :: nargs
   character(len=10)    :: ns,nexp
   integer              :: m, step
   integer(ik)          :: n, nsteps
   real, dimension(2)   :: pos, move
   real                 :: distance, avg

   nargs=command_argument_count()
   if ( nargs .ne. 1 ) then
      write(*,*) 0, 0., 0.
      stop
   else
      call get_command_argument(1,ns)
      read(ns,'(i10)') nsteps
   endif

   call set_random_seed()
   pos=[0.,0.]

   avg=0.

   do n=1,nsteps

      step=randint(1,4)
      if (step==1) then
         move=[0.,1.]
      else if (step==2) then
         move=[0.,-1.]
      else if (step==3) then
         move=[1.,0.]
      else
         move=[-1.,0.]
      endif
      pos=pos+move

   enddo

   distance=sqrt(pos(1)**2+pos(2)**2)

   write(*,*) nsteps, sqrt(real(nsteps)), distance

end program
