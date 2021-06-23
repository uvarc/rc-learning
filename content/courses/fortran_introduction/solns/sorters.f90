module sorters
implicit none

contains

subroutine bubblesort(array,length)
real, dimension(length),intent(inout)   :: array
integer,                intent(in)      :: length

real                      :: temp
integer                   :: i,j,n

! Sorts a vector using bubble sort.  Returns sorted vector in place.
! Sorts in ascending order.

  do i=1,length
     do j=1,length
        if (array(i) .lt. array(j)) then
            temp = array(i)
            array(i) = array(j)
            array(j) = temp
        endif
     enddo
  enddo

return
end subroutine bubblesort

subroutine pbsort(x,perms,length)
real,    dimension(length), intent(inout) :: x
integer, dimension(length), intent(out)   :: perms
integer,                    intent(in)    :: length

real                        :: temp
integer                     :: itemp
integer                     :: i,j

! Sorts a vector with a simple bubble sort.  Returns sorted vector in place 
! along with the permutation vector.  Sorts in ascending order.

  do i=1,length
     perms(i)=i
  enddo

  do i=1,length
     do j=1,length
        if (x(i) .lt. x(j)) then
            temp  = x(i)
            itemp = perms(i)
            x(i) = x(j)
            perms(i)=perms(j)
            x(j) = temp
            perms(j)=itemp
        endif
     enddo
  enddo

return

end subroutine pbsort


subroutine shellsort(array,length)
real, dimension(length), intent(inout) :: array
integer,                 intent(in)    :: length

real                    :: temp
integer                 :: inc,lower,upper
integer                 :: i,j

! Sorts an array with indices running from 1 to length in place using Shell sort

lower=lbound(array,1)
upper=ubound(array,1)

inc=length/2

! We continue until the stride is the lower bound

do while (inc > 0)
   do i=inc+lower,upper
      temp=array(i)
      j=i
      do while ( j > inc .and. array(j-inc) > temp )
         array(j)=array(j-inc)
         j=j-inc
      enddo
      array(j)=temp
   enddo
   if ( inc == 2 ) then
      inc=1
   else
      inc=int(real(inc)/2.2)
   endif
enddo

end subroutine shellsort


subroutine pshellsort(array,perms,length)
real, dimension(length),  intent(inout) :: array
integer,dimension(length),intent(out)   :: perms
integer,                  intent(in)    :: length

real                    :: temp
integer                 :: itemp
integer                 :: inc,lower,upper
integer                 :: i,j

! Sorts an array with indices running from 1 to length in place using Shell sort

lower=lbound(array,1)
upper=ubound(array,1)

do i=1,length
   perms(i)=i
enddo

inc=length/2

! We continue until the stride is the lower bound

do while (inc > 0)
   do i=inc+lower,upper
      temp=array(i)
      itemp=perms(i)
      j=i
      ! This only works if the compiler short-circuits
      do while ( j > inc .and. array(j-inc) > temp )
         array(j)=array(j-inc)
         perms(j)=perms(j-inc)
         j=j-inc
      enddo
      array(j)=temp
      perms(j)=itemp
   enddo
   if ( inc == 2 ) then
      inc=1
   else
      inc=int(real(inc)/2.2)
   endif
enddo

end subroutine pshellsort

end module
