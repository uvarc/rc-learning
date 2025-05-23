program passit
implicit none

   integer, parameter   :: n=4
   integer, dimension(2):: sh
   real, dimension(4*n)   :: V
   real, dimension(n,n) :: A
   real, allocatable, dimension(:,:) :: B
   integer              :: i

   interface
      subroutine mysub1(X)
         implicit none
         real,dimension(:,:), intent(inout) :: X
      end subroutine
      subroutine mysub2(X,n)
         implicit none
         integer,             intent(in)    :: n
         real,dimension(n,n), intent(inout) :: X
      end subroutine
   end interface  

   V=[(real(i),i=1,16)]
   sh=[4,4]
   A=reshape(V,sh)

   allocate(B(n,n))
   B=A+2.

   call mysub1(A)
   call mysub2(A,n)
   call mysub1(B)
   call mysub2(B,n)

end program

subroutine mysub1(X)
   implicit none
   real,dimension(:,:), intent(inout) :: X
   
   print *, shape(X), X(2,2)
end subroutine

subroutine mysub2(X,n)
   implicit none
   integer,             intent(in)    :: n
   real,dimension(n,n), intent(inout) :: X
   
   print *, shape(X), n, X(2,2)
end subroutine
