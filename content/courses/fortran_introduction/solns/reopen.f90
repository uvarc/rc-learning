program reopen
implicit none

   character(len=50) :: filename
   integer           :: i,item1,item2,item3

   filename="mydata.txt"

   open(unit=10,file=filename)
   
   do i=1,12,3
      write(10,'(i4,a,i4,a,i4)',advance='no') i,",",i+1,",",i+2
      write(10,*)
   enddo

   close(10)

   open(unit=11,file=filename)
   do i=1,4
      read(11,*) item1,item2,item3
      item1=item1+1
      item2=item2+1
      item3=item3+1
      print *, item1,item2,item3
   enddo

end program


