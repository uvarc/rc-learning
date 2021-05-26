program arrayinds
integer, dimension(1)              :: maxtemp
integer, dimension(28)             :: feb
real,    dimension(365)            :: temps
real,    allocatable, dimension(:) :: x
integer, allocatable, dimension(:) :: nums
logical, allocatable, dimension(:) :: is_max
character(len=7),allocatable, dimension(:) :: dates
character(len=3),dimension(12 )    :: months
integer, dimension(12 )            :: mons=[31,28,31,30,31,30,31,31,30,31,30,31]
character(len=3)                   :: month
character(len=2)                   :: day_of_month
integer                            :: ndays
integer                            :: i,m,mday,day
real, parameter                    :: pi=4.0*atan(1.0)

   months=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov', &
           'Dec']

   ndays=size(temps)
   allocate(nums(ndays),x(ndays),dates(ndays),is_max(ndays))
   nums=[(real(i),i=1,size(temps))]
   x=nums/365.
   ! Dates as characters
   day=0
   do m=1,12
       month=months(m)
       do mday=1,mons(m)
          day=day+1
          write(day_of_month,'(i2)') mday
          dates(day)=day_of_month//'-'//month
       enddo
   enddo
   !Very artificial temperatures (in deg C)
   temps=35.*sin(x*pi)
   print *, "Temperatures for February"
   feb=[(i,i=32,59)]
   write(*,*) temps(feb)
   maxtemp=maxloc(temps)
   print *, "Maximum temp was at ",dates(maxtemp)

   is_max = temps==maxval(temps)
   print *, "Maximum temperature(s) were at"
   do day=1,size(temps)
      if (is_max(day)) then
         write(*,'(a)',advance='no') dates(day)
      endif
   enddo
   write(*,*)

end program
