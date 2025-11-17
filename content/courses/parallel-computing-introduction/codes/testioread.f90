program testread

implicit none

integer :: nlines

integer :: i, j, k, l, n
integer :: ios,sz
integer :: iunit=2
logical :: file_exists

character(len=64) :: fname,iomerr

if (command_argument_count() < 1) then
    print *,'Usage: read [filename]'
else 
    call get_command_argument(1, fname)
    open(2, file=trim(fname), form='unformatted',access='stream', convert='little_endian')
endif

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

   print *, 'Found ',nlines,' lines'

   rewind(iunit)

!   allocate(time(nlines),obs(nlines))

   do n=1,nlines
!      read(iunit,*) vals(:)
   enddo

end program testread

