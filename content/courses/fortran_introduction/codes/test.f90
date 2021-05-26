program test
implicit none
   integer a
   dimension a(5,5)
   integer i, j, n, m, y
   integer k,l
   interface
      subroutine absmax(a,i,k,y)
      implicit none
      integer, dimension(:,:), intent(in) :: a
      integer,                 intent(out):: i,k
      integer,                 intent(out):: y
      end subroutine
   end interface
   do i=1,5
      do j=1,5
            a(i,j)=i*(j+1)/i
      enddo
   enddo
   call absmax(a,k,l,y)
   print *, y,k,l
   y=maxval(a)
   print*, maxloc(a)
   print *, y
end

subroutine absmax(a,i,k,y)
implicit none
!  The absolute greatest element of array A of size NxM is computed and returned
!  in Y. The corresponding location subscripts are returned in I and K.
   integer, dimension(:,:), intent(in) :: a
   integer,                 intent(out):: i,k
   integer,                 intent(out):: y
   integer                             :: p,q

   y=0; i=1; k=1
   do p=1,size(a,1)
      do q=1,size(a,2)
         if (abs(a(p,q)) > y) then
            y=abs(a(p,q))
            i=p
            k=q
         endif
      enddo
   enddo
end subroutine

