program console_io
implicit none
real   ::  x,y

   write(*,*) "Please enter a number."
   read(*,*)  x
   write(*,*) "Please enter another number."
   read(*,*)  y

   print *, "The product of your numbers is ",x*y
   write(*,*,decimal='COMMA') "European style:",x*y

end program
