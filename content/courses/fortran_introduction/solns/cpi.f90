program compute_cpi
!  This program reads Consumer Price Index data 
implicit none

   character(len=80)                  :: infile,outfile
   character(len=4)                   :: year_char
   real,    dimension(:), allocatable :: cpi
   real                               :: ratio, tv_price
   integer, dimension(:), allocatable :: years
   integer                            :: my_year
   integer                            :: nlines,nyears
   integer                            :: ios, inunit, outunit
   integer                            :: nargs
   integer                            :: n
   logical                            :: file_exists
   logical                            :: check_year

   nargs=command_argument_count()
   if (nargs .lt. 2) then
      stop "No data file or year specified"
   else if (nargs .ne. 2) then
      check_year=.false.
   else
      check_year=.true.
      call get_command_argument(1,infile)
      call get_command_argument(2,year_char)
      read(year_char,'(i4)') my_year
   endif

   inquire(file=infile,exist=file_exists)

   if (file_exists) then
      inunit=10
      open(inunit,file=infile,iostat=ios)
      if ( ios .ne. 0 ) then
         stop "Unable to open data file"
      endif
   else
      stop "Data file not found"
   endif

   nlines=0
   do
      read(inunit,*,end=10)
      nlines=nlines+1
   enddo
10 continue
   rewind(inunit)

   !Read and discard header
   read(inunit,*)

   ! Subtract header line
   nyears=nlines-1

   allocate(years(nyears),cpi(nyears))

   !Read data
   do n=1,nyears
      read(inunit,*,iostat=ios) years(n),cpi(n)
      if ( ios .ne. 0 ) stop "Error reading file"
   enddo

   do n=1,nyears
      if ( check_year .and. years(n)==my_year ) then
          ratio=cpi(nyears)/cpi(n)
          write(*,'(a,i4,a,f8.2)')  "The inflation factor since ",my_year,     &
                                    " is about",ratio
      endif
      if ( years(n)==1954 ) then
           tv_price=1295.*cpi(nyears)/cpi(n)
           write(*,1) "A color television in 1954 would cost about ",          &
                      tv_price," in today's dollars"
      endif
   enddo
1  format(a,f8.2,a,f8.2,a)

end program
