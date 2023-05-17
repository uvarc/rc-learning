program blocker
   implicit none
   real    ::  x,y

      x=10.5
      y=12.0

      block
         real  :: x
         x=9.1
         write(*,*) "In the block", x,y
      end block
      write(*,*) "Outside the block", x,y

      call mysub(11)
end program

subroutine mysub(n)
   implicit none
   integer :: n,ns
      if (n>0) then
         block
            integer :: ns
            ns = sqrt(real(n))
            print *, ns
         end block
         print *, ns
      endif
      if (n>0) then
         block
            integer :: ns=4
            print *,ns
         end block
      end if
end subroutine 
