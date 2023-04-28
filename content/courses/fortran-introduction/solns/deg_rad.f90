program deg2rad
implicit none

real                            :: dtheta,rtheta
real                            :: start_val,end_val,incr
real, dimension(:), allocatable :: degrees,rads
real                            :: angle
integer                         :: n,nvals

interface
   elemental real function deg_to_rad(theta)
      implicit none
      real, intent(in) :: theta
   end function
end interface

   dtheta=15.
   rtheta=deg_to_rad(dtheta)
   print *, "An angle of ",dtheta," in degrees is ",rtheta," radians."

   start_val=0.
   end_val  =90.
   incr     =5.
   nvals=90./incr+1

! Loop
   do n=1,nvals
      angle=start_val+incr*(n-1)
      write(*,'(f12.4,x,f12.4)') angle,deg_to_rad(angle)
   enddo

! Array
  degrees=[(start_val+incr*(n-1),n=1,nvals)]
  rads=deg_to_rad(degrees)
  print *, rads

end program

elemental real function deg_to_rad(theta)
      implicit none
      real, intent(in) :: theta
      real             :: pi

      pi =4.0*atan(1.0)
      deg_to_rad=theta*pi/(180)
end function
