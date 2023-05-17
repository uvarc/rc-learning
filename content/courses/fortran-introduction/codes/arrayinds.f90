program arrayinds
integer, dimension(1)           :: maxtemp
integer, dimension(28)          :: feb
real,    dimension(365)         :: x, temps
integer, dimension(365)         :: nums
character(len=7),dimension(365) :: dates
character(len=3),dimension(12 ) :: months
integer, dimension(12 )         :: mons=[31,28,31,30,31,30,31,31,30,31,30,31]
character(len=3)                :: month
character(len=2)                :: day_of_month
integer                         :: i,m,mday,day
real, parameter                 :: pi=4.0*atan(1.0)

   months=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov', &
           'Dec']
   nums=[(real(i),i=1,365)]
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

end program
