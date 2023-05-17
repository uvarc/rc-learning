program looper
! Set up some loops
implicit none

   integer :: i,n,L,M
   real    :: w,x,z

   print *, "Part 1"
   do i=0,20,2
      print *, i
   enddo

   print *, "Part 2"
   n=1
   do while (n<121)
      if (mod(n,2)==0) then
         n=n+3
      else
         n=n+5
      endif
      print *, n
   enddo

   print *, "Part 3"
! Remember that case doesn't matter, but capital L doesn't look like the digit one
   L=50
   M=25
   w=9.
   z=13.

   x=0.
   do i=1,L
      if (i<M) then
         x=x+11
      endif

      if (x>w .and. x<z) cycle

      if (x>100.) exit
   enddo

   print *, x

end program


