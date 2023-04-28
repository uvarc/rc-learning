program read_arg
implicit none

   integer            :: nargs
   character(len=80)  :: infile

   nargs=command_argument_count()
   if (nargs .lt. 1) then
      stop "No file name specified"
   else
      call get_command_argument(1,infile)
   endif

   write(*,'(a,a)') "You entered a file name ",trim(infile) 

end program

