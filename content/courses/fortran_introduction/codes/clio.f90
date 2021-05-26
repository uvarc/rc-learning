program console_io
implicit none
character(len=15) :: xvar, yvar
real              ::  x,y
integer           :: nargs

   nargs=command_argument_count()
   if (nargs .ne. 2) then
      stop "Too few arguments."
   else
      call get_command_argument(1,xvar)
      call get_command_argument(2,yvar)
   endif

   read(xvar,'(f15.8)') x
   read(yvar,'(f15.8)') y

   print *, "The product of your numbers is ",x*y
   write(*,*,decimal='COMMA') "European style:",x*y

end program
