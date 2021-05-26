module things
use precisions
type something
   !variables
   contains
      private
      procedure :: rdiag
      procedure :: ddiag
      generic,public :: diag=>rdiag,ddiag
end type

contains
   function rdiag(this,x)
   class(something) :: this
   real(sp) :: x
   end function
   function ddiag(this)
   class(something) :: this
   real(dp) :: x
   end function


end module
