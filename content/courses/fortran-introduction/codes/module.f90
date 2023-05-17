module mymod
implicit none
integer   ::Nmax=100000

   contains

   subroutine mysub(a,x)
      real, dimension(:), intent(in) :: a
      real,               intent(out):: x
      real, dimension(Nmax)          :: b

         b=x

   end subroutine

   function myfunc(x,y)
      real                            :: myfunc
      real,intent(in)                 :: x,y
      myfunc=x**2+y
   end function

end module

