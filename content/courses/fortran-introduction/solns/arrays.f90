program arrays
implicit none
real, dimension(10,10) :: A
integer                :: i, j
real, dimension(10,10) :: W, Z

   do j=1,10
      do i=1,10
         A(i,j)=0.1*i+0.2*j
      enddo
   enddo
   print *, size(A)
   print *, shape(A)

   W=1.0
   do j=1,10
      do i=1,10
         Z(i,j)=i+j
      enddo
   enddo
   Z(2,4)=1.1

   where (Z<7)
      W=Z
   endwhere
   print *,W(2,4)

end program

