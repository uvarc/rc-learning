program inflation
!  This program reads Consumer Price Index data and computes an approximation
!  to the inflation rate

implicit none

   character(len=80)                  :: infile,outfile
   character(len=4)                   :: year_char
   real,    dimension(:), allocatable :: cpi
   real,    dimension(:), allocatable :: inflat
   integer, dimension(:), allocatable :: years
   integer                            :: nlines,nyears
   integer                            :: ios, inunit, outunit
   integer                            :: nargs
   integer                            :: n

   interface 
      function unit_number()
          integer               :: unit_number
      end function unit_number
   end interface

   infile="cpi.csv"
   inunit=unit_number()
   open(inunit,file=infile)

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

   inflat=0.0
   do n=1,nyears-1
      inflat(n)=(cpi(n+1)-cpi(n))/12.
    enddo

   outfile="inflation.csv"
   outunit=unit_number()
   open(outunit,file=outfile,iostat=ios)
   if ( ios .ne. 0 ) stop "Unable to open output file"

  ! Write a header
   write(outunit,*) "year,","inflation rate"
  ! Write out values for plotting
   do n=1,nyears-1
      write(outunit,'(i4,a,f8.2)') years(n),",",inflat(n)
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

