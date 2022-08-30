!:.............................................................................:
!:                                                                             :
!:               	     Random Number Utilities                           :
!:                                                                             :
!:.............................................................................:
!: Author: Katherine Holcomb

module Random
  implicit none

  ! Comment out one or the other when the module is incorporated into a code.

  ! Single precision
  !integer, parameter   :: rk = kind(1.0)

  ! Double precision
  integer, parameter   :: rk = kind(1.0d0)


contains

  subroutine get_random_seed(seed)
    ! To use this subroutine, seed must be declared allocatable in the
    ! calling unit.
    integer, dimension(:), allocatable, intent(out)  :: seed

    integer                                          :: isize

    call random_seed(size=isize)
    if (.not. allocated(seed)) allocate(seed(isize))

    call random_seed(get=seed)

  end subroutine get_random_seed


  subroutine set_random_seed(seed)
    ! Sets all elements of the seed array
    integer, optional, intent(in)       :: seed

    integer                             :: isize
    integer, dimension(:), allocatable  :: iseed
    integer, dimension(8)               :: idate
    integer, parameter                  :: default_seed=2345678

    call get_random_seed(iseed)

    if ( .not. present(seed) ) then
       call date_and_time(values=idate)
       ! idate(8) contains millisecond
       if ( all(iseed .ne. 0) ) then
          iseed = iseed * (idate(8))
       else
          iseed = default_seed * (idate(8))
       endif
    else
       iseed=int(seed)
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


  function nrand(mean,std,seed)
    ! Returns a normally-distributed random number with given mean and 
    ! standard dev.
    real(rk)                         :: nrand
    real(rk), optional, intent(in)   :: mean,std
    real(rk), optional, intent(in)   :: seed

    real(rk)                         :: rnd
    integer                          :: iseed
    real(rk)                         :: mn, st
    real(rk)                         :: x1, y1, x2, y2, w

    if ( present(mean) ) then
       mn = mean
    else
       mn = 0.0_rk
    endif

    if ( present(std) ) then
       st = std
    else
       st = 1.0_rk
    endif

    ! Box-Muller polar method
1   continue

    if ( present(seed) ) then
       iseed=int(seed)
       x1=urand(-1.0_rk,1.0_rk,seed)
       x2=urand(-1.0_rk,1.0_rk,seed)
    else
       x1=urand(-1.0_rk,1.0_rk)
       x2=urand(-1.0_rk,1.0_rk)
    endif
    w = x1**2 + x2**2
    if ( w .eq. 0.0_rk .or. w .gt. 1.0_rk ) go to 1

    w = sqrt( (-2.0_rk * log( w ) ) / w )
    y1 = x1*w
    y2 = x2*w
    ! Pick one, adjust its mean and std
    nrand = y1*st+mn

    return
  end function nrand


  function randint(n,m)
    ! Returns a random integer between n and m.  
    integer                 :: randint
    integer, intent(in)     :: n,m

    randint=ceiling(urand(real(n-1,rk),real(m,rk)))

  end function randint


  function choice(a)
    ! Returns a random selection from array a
    integer                             :: choice
    integer, dimension(:), intent(in)   :: a
    integer                             :: n
      n=size(a)
      choice=a(randint(1,n))
    end function choice


  subroutine shuffle(a)
    integer, dimension(:), intent(inout) :: a
    integer                              :: i
    integer                              :: randpos,tempval
    real                                 :: rnd
    
    do i=size(a),1,-1
       call random_number(rnd)
       randpos   =int(rnd*i)+1
       tempval   =a(randpos)
       a(randpos)=a(i)
       a(i)      =tempval
    enddo

  end subroutine shuffle


end module Random
