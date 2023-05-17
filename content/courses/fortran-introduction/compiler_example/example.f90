program example
implicit none

   real ::  x, y
   real ::  res

   interface
      real function adder(x,y)
          implicit none
          real, intent(in) :: x,y
      end function
   end interface

   x=4.3; y=11.7
   res=adder(x,y)
   print *, res

end program
