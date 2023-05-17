program day_of_week
!*******************************************************************************
  ! This program computes the day of the week given a date in the 
  ! Gregorian calendar.
  !
  ! Author:    K. Holcomb
  ! Changelog: 2013-06-05 Initial version
  !            2014-01-29 Bug fix to correct leap_year test for century 
  !            2014-01-29 Added a loop for user input
  !            2015-01-29 Modification to use only conditionals (no arrays)
!*******************************************************************************
   implicit none

   integer                          ::  max_year, min_year
   integer                          ::  day, month, year, century
   integer                          ::  D, M, Y, C, L, W
   logical                          ::  leap_year, century_leap_year


   day=2
   month=3
   year=2016

   D=day
   century=100*(int(year)/100)
   Y=year-century

   century_leap_year = mod(century,400)==0

   leap_year=.false.
   if (Y>0) then
      leap_year = mod(year,4)==0
   else
      if (century_leap_year) leap_year=.true.
   endif

   L = int(Y)/4

   if (century_leap_year) L=L+1

   if (leap_year .and. month<3) L=L-1

   if (month==1 .or. month==10) then
      M=0
   elseif (month==2 .or. month==3 .or. month==11) then
      M=3
   elseif (month==4 .or. month==7) then
      M=6
   elseif (month==5) then
      M=1
   elseif (month==6) then
      M=4
   elseif (month==8) then
      M=2
   else
      M=5
   endif

   if     ( century==1400 .or. century==1800 .or. century==2200 ) then
      C=2
   elseif ( century==1500 .or. century==1900 .or. century==2300 ) then
      C=0
   elseif ( century==1600 .or. century==2000 .or. century==2400 ) then
      C=5
   elseif ( century==1700 .or. century==2100 .or. century==2500 ) then
      C=4
   else
      print *, "This algorithm doesn't cover the century requested"
   endif

   W=mod((C+Y+L+M+D),7)

   !Example of select case
   select case(W)
   case(0)
      print *, 'Sunday'
   case(1)
      print *, 'Monday'
   case(2)
      print *, 'Tuesday'
   case(3)
      print *, 'Wednesday'
   case(4)
      print *, 'Thursday'
   case(5)
      print *, 'Friday'
   case(6)
      print *, 'Saturday'
   end select 

end program

