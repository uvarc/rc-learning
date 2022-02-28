program get_animal
implicit none

   integer          :: ans
   character(len=8) :: text

   do 
      write(*,'(a)',advance='no') "Please enter a digit 1 or 2, or 0 to exit:"
      read(*,*) ans

      if (ans==0) then
         exit
      else if (ans==1) then
         print *, "zebra"
      else if (ans==2) then
         print *, "kangaroo"
      else
         print *, "not found"
      endif
   enddo

end program
