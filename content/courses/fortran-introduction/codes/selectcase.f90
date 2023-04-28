program selectcase
implicit none

   integer :: i
   integer :: x 
   real    :: y

   x=15

   select case (x)
      case  (:0)
         y=-real(x)
      case (1)
         y=x+3.
      case (2:9)
         y=float(x)/2.
      case (10:20)
         y=float(x)/3.
      case default
         y=0.
   end select

   print *, x, y

end program
