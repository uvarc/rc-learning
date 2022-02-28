program print_pi
implicit none

   integer, parameter :: rk=kind(1.0)
   !integer, parameter :: rk=kind(1.0d0)

   real(rk)           :: pi=4.0_rk*atan(1.0_rk)

   write(*,'(g12.6)') pi
   write(*,'(e12.3)') pi
   write(*,'(es12.3)') pi
   write(*,'(es15.8)') pi

end program

