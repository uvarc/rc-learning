program inflation
!  This program reads Consumer Price Index data and computes an approximation
!  to the inflation rate
!  Author    : K. Holcomb
!  Changelog:: Initial code 20130210

implicit none

   character(len=80)                  :: infile,outfile
   character(len=4)                   :: year_char
   character(len=12)                  :: amount_char
   real,    dimension(:), allocatable :: cpi
   real,    dimension(:), allocatable :: inflat
   real                               :: ratio, tv_price
   real                               :: my_amount=0.0
   integer, dimension(:), allocatable :: years
   integer                            :: my_year
   integer                            :: nlines,nyears
   integer                            :: ios, inunit, outunit
   integer                            :: nargs
   integer                            :: cpi_index
   integer                            :: n
   logical                            :: file_exists

   interface 
      function unit_number()
          integer               :: unit_number
      end function unit_number
   end interface

   nargs=command_argument_count()
   if (nargs .lt. 2) then
      stop "Usage: datafile year <amount>"
   else if (nargs .eq. 2) then
      call get_command_argument(1,infile)
      call get_command_argument(2,year_char)
      read(year_char,'(i4)') my_year
   else
      call get_command_argument(1,infile)
      call get_command_argument(2,year_char)
      call get_command_argument(3,amount_char)
      read(year_char,'(i4)') my_year
      read(amount_char,*) my_amount
   endif

   inquire(file=infile,exist=file_exists)

   if (file_exists) then
      inunit=unit_number()
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
   allocate(inflat(nyears-1))

   !Read data
   do n=1,nyears
      read(inunit,*,iostat=ios) years(n),cpi(n)
      if ( ios .ne. 0 ) stop "Error reading file"
   enddo

   if (my_year > years(nyears) .or. my_year < years(1)) then
      stop "Requested year outside of data range."
   endif

   cpi_index=my_year-years(1)+1
   ratio=cpi(nyears)/cpi(cpi_index)

   if (my_amount==0) then 
       write(*,'(a,i4,a,f0.2)') "The inflation factor since ",my_year,         &
                                " is about ",ratio
   else
       write(*,'(f0.2,a,i4,a,f0.2,a,i4)') my_amount," in ",my_year,            &
                             " is about ",ratio*my_amount," in ",years(nyears)
   endif

   inflat=0.0
   do n=1,nyears-1
      inflat(n)=(cpi(n+1)-cpi(n))/12.
    enddo

   outfile="inflation.csv"
   outunit=unit_number()
   open(outunit,file=outfile,iostat=ios)
   if ( ios .ne. 0 ) stop "Unable to open output file"

  ! Write out values for plotting
   do n=1,nyears
      write(outunit,'(i4,a)',advance='no') years(n)
      write(outunit,*) inflat(n)
   enddo

end program inflation


function unit_number()
   integer               :: unit_number
   integer               :: base_unit=10
   logical               :: first=.true.

   if ( first ) then
       unit_number=base_unit
       first=.false.
   else
       unit_number=unit_number+1
   endif

end function unit_number

