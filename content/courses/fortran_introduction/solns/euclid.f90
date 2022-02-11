program euclid
implicit none

   real, dimension(2)       :: p1, p2, p3
   integer                  :: winner

   interface
      subroutine compare_points(p1,p2,p3,winner)
      implicit none
         real, dimension(2), intent(in)  :: p1, p2, p3
         integer,            intent(out) :: winner
      end subroutine
   end interface

   !Point 1
   write(*,'(a)',advance='no') "Please enter the first point, two reals separated by a comma: "
   read(*,*) p1(1), p1(2)

   !Point 2
   write(*,'(a)',advance='no') "Please enter the second point, two reals separated by a comma: "
   read(*,*) p2(1), p2(2)

   !Point 3
   write(*,'(a)',advance='no') "Please enter the third point, two reals separated by a comma: "
   read(*,*) p3(1), p3(2)

   call compare_points(p1,p2,p3,winner)

   if (winner==1) then
      print *, "Point 1 is closer to Point 3"
   else if (winner==2) then
      print *, "Point 2 is closer to Point 3"
   else if (winner==0) then
      print *, "The distance is the same"
   else
      print *, "Error"
   endif

end program

real function euclidean(p1,p2)
   implicit none
   real, dimension(2), intent(in) :: p1, p2

   euclidean=sqrt((p2(1)-p1(1))**2+(p2(2)-p2(1)))
end function

subroutine compare_points(p1,p2,p3,winner)
   implicit none
   real, dimension(2), intent(in)  :: p1, p2, p3
   integer,            intent(out) :: winner
   real                            :: dist13, dist23
   interface
      real function euclidean(p1,p2)
         implicit none
         real, dimension(2), intent(in) :: p1, p2
      end function
   end interface

   winner=-1

   dist13=euclidean(p1,p3)
   dist23=euclidean(p2,p3)

   if (dist13==dist23) winner=0
   if (dist13<dist23)  winner=1
   if (dist13>dist23)  winner=2

end subroutine
   

