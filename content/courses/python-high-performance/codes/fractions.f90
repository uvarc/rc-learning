module Fractions
   implicit none

   contains

      function adder(n1,d1,n2,d2)
         integer, dimension(2)    :: adder
         integer, intent(in)      :: n1, d1, n2, d2
         integer                  :: num, denom 
         if ( d1 .ne. 0 .and. d2 .ne. 0 ) then
               num=n1*d2+n2*d1
               denom=d1*d2
               adder=(/num,denom/)
            else
               adder=(/0,0/)
            endif
      end function adder
         
end module Fractions
