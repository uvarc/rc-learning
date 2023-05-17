program ieee
use, intrinsic :: ieee_arithmetic
implicit none
   real           :: x,y

   x=-1.
   y=sqrt(x)
   if ( ieee_is_nan(y) ) then
      print *, "NaN"
   else
      print *, y
   endif

end program


