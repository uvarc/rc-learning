program file_io_demo
implicit none
integer :: iunit, ios, nlines, sz
integer :: n
logical :: file_exists
character(len=128) :: fname, iomerr
integer :: item1, item2, item3

   fname="mydata.txt"
   iunit=10

   inquire(file=fname,exist=file_exists,size=sz)
   if (sz==0) then
      stop "Empty file"
   else if ( .not. file_exists ) then
      stop "File "//trim(fname)//" not found."
   endif

   open(iunit,file=fname,iostat=ios,status='UNKNOWN')
      if (ios /= 0) then
         stop "Can't open file "//trim(fname)
      endif
   nlines=0
   do
   read(iunit,*,iostat=ios,iomsg=iomerr,end=1)
      if (ios /= 0) then
         stop "Error reading file:"//trim(iomerr)
      else
          nlines=nlines+1
      endif
   end do
1  continue

   rewind(iunit)

   do n=1,nlines
      read(iunit,*) item1, item2, item3
   enddo

end program
