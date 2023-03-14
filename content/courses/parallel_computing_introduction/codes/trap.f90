program trapezoid
implicit none
                                                                                
! Calculate a definite integral using trapezoid rule
                                                                                
real    :: a, b
integer :: n

real    :: h, integral
real    :: x
integer :: i, nargs
character(len=16) lb, ub, ns

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

  nargs=command_argument_count()
  if ( nargs .ne. 3 ) then
     stop "Usage: arguments are lower bound, upper bound, number of steps"
  else
     call get_command_argument(1,lb)
     read(lb,'(f16.9)') a
     call get_command_argument(2,ub)
     read(ub,'(f16.9)') b
     call get_command_argument(3,ns)
     read(ns,'(i10)') n
  endif

   h=(b-a)/n
   integral=trap(f,a,b,h,n)

   print *, integral
                                                                                
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

