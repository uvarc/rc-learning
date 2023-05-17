program fibonnaci
   implicit none
   integer   ::  n

   interface
      integer function ifib(n)
         implicit none
         integer, intent(in)   :: n
      end function
      recursive function rfib(n) result(fib)
         implicit none
         integer               :: fib
         integer, intent(in)   :: n
      end function
   end interface

   write(*,'(a)',advance='no') "Please enter an integer >=0:"
   read(*,*) n
   print *, ifib(n), rfib(n)
end program
   
integer function ifib(n)
! Iterative
   implicit none
   integer, intent(in)   :: n
   integer               :: nm1, nm2, nval
   integer               :: i

   if (n < 0) then
      write(*,*) "Illegal term number."
      ifib=-1
      return
   endif

   nm1=0
   nm2=1
      
   if (n==0) then
      ifib=nm1
      return
   else if (n==1) then
      ifib=nm2
      return
   else
      do i=1,n
        nval=nm1+nm2
        nm2=nm1
        nm1=nval
      enddo
      ifib=nval
   endif
end function
   
recursive function rfib(n) result(fib)
! recursive, F2003 standard
   implicit none
   integer               :: fib
   integer, intent(in)   :: n
   integer               :: nm1, nm2, nval
   integer               :: i

   if (n < 0) then
      write(*,*) "Illegal term number."
      fib=-1
      return
   endif

   nm1=0
   nm2=1
      
   if (n==0) then
      fib=nm1
      return
   else if (n==1) then
      fib=nm2
      return
   else
      fib=rfib(n-1)+rfib(n-2)
   endif
end function
