program trapezoid
use mpi
implicit none
                                                                                
! Calculate a definite integral using trapezoid rule
                                                                                
real    :: a, b
integer :: n

real    :: h, integral
real    :: x
integer :: i, nargs
character(len=16) lb, ub, ns

real   :: local_a, local_b, total 
integer:: rank, nprocs, ierr, errcode
integer:: local_n
logical:: shutdown=.false.

interface 
  real function trap(f,a,b,h,n)
      implicit none
      real,    intent(in)   :: a, b, h
      integer, intent(in)   :: n
      interface
         real function f(x)
         implicit none
         real, intent(in) :: x
         end function
       end interface
   end function
   real function f(x)
        implicit none
        real, intent(in) :: x
   end function
end interface

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)

  if (rank==0) then
      nargs=command_argument_count()
      if ( nargs .ne. 3 ) then
         write(*,*) "Usage: arguments are lower bound, upper bound, number of steps"
         shutdown=.true.
      else
         call get_command_argument(1,lb)
         read(lb,'(f16.9)') a
         call get_command_argument(2,ub)
         read(ub,'(f16.9)') b
         call get_command_argument(3,ns)
         read(ns,'(i10)') n
         shutdown=.false.
      endif
      if (.not. shutdown) then
      !Don't bother to distribute number of steps, just stop if not even
         if (mod(n,nprocs) /=0) then
             write(*,*) "Number of processes must evenly divide number of steps"
             shutdown=.true.
         endif
      endif
   endif

   call MPI_Bcast(shutdown,1,MPI_LOGICAL,0,MPI_COMM_WORLD,ierr)
   if (shutdown) then
      call MPI_Finalize(ierr)
      stop 
   endif

   call MPI_Bcast(a,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)
   call MPI_Bcast(b,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)
   call MPI_Bcast(n,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

   h=(b-a)/n
   local_n = n/nprocs

   local_a = a + rank*local_n*h
   local_b = local_a + local_n*h
   integral = trap(f, local_a, local_b, h, local_n)

   call MPI_Reduce(integral,total,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)

   if (rank .eq. 0) then
      print *, total
   endif

   call MPI_Finalize(ierr)

end program

real function trap(f, a, b, h, n)
   implicit none
   real,    intent(in)   :: a, b, h
   integer, intent(in)   :: n

   interface
     real function f(x)
        implicit none
        real, intent(in) :: x
     end function
   end interface

   real   :: x
   integer:: i
   real   :: integral

   integral = (f(a) + f(b))/2.0

   x=a
   do i=1, n-1
      x = x+h
      integral = integral + f(x)
   enddo

   trap   = integral*h
   return

end function
                                                                                
real function f(x)
  implicit none
  real, intent(in):: x
     f=sin(x)
end function

