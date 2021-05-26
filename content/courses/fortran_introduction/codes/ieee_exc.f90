program ieee
use, intrinsic :: ieee_exceptions
implicit none
   type(ieee_flag_type), dimension(3) :: flag
   logical        :: halt=.true.
   real           :: x,y

   flag=ieee_usual
   call ieee_set_halting_mode(flag,halt)

   x=-1.
   y=sqrt(x)
   print *, y

end program


