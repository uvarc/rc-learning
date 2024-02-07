module Random
  implicit none
  ! Comment out one or the other when the module is incorporated into a code.

  ! Single precision
  integer, parameter   :: rk = kind(1.0)

  ! Double precision
  !integer, parameter   :: rk = kind(1.0d0)

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

  function randint(n,m)
    ! Returns a random integer between n and m.
    integer                 :: randint
    integer, intent(in)     :: n,m

    randint=ceiling(urand(real(n-1,rk),real(m,rk)))

  end function randint

end module random

program manager_worker
use random
use mpi

   integer :: i, n

   integer :: nprocs, rank, sender, ierr
   integer, dimension(MPI_STATUS_SIZE) :: status;

   integer ::  done=0
   real    ::  my_result, total=0.
   real, dimension(:), allocatable :: results
   integer, dimension(:), allocatable :: seed

   interface
        function do_work() result(my_result)
        use random
        real :: my_result
      end function do_work
   end interface

   call MPI_Init(ierr)
   call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

   allocate(results(nprocs))
   results=0.

   ! The topic of handling random-number generation in parallel programs is
   ! an issue in applied mathematics, and beyond our scope here. We just
   ! use the rank to spread out the seeds some.
   call get_random_seed(seed)
   seed=seed*(rank+1)
   call set_random_seed(seed(1))

   if (rank==0) then
       do i=1,nprocs-1
          call MPI_Recv(
          sender=
          results(sender)=my_result
          done=1;
          call MPI_Send(done,1,MPI_INTEGER,sender,0,MPI_COMM_WORLD,ierr)
       enddo
   else 
       do n=1,nprocs-1
          if (rank==n) then
             my_result=do_work();
             call MPI_Send(my_result,1,MPI_REAL,0,rank,MPI_COMM_WORLD,ierr)
             call MPI_Recv(done,1,MPI_INTEGER,0,MPI_ANY_TAG,MPI_COMM_WORLD,status,ierr)
          endif
       enddo
   endif

   total=sum(results)
   if (rank==0) then
       write(*,*) "The final result is",total
   endif

   call MPI_Finalize(ierr)

end program

function do_work() result(my_result)
    use random
    real :: my_result
    integer nsteps
    !hardcoded bounds for convenience
    nsteps=randint(10000,30000);

    my_result=0.
    do i=1,nsteps
        my_result=my_result+i;
    enddo
end function
