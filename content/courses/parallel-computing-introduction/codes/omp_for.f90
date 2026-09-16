
use omp_lib

   integer, parameter    :: M=10;
   integer, dimension(M) :: a, b
   real,    dimension(M) :: c
   integer               :: i,j
   integer               :: low, high

   a=[1,3,5,7,2,3,4,5,6,7]
   b=[9,5,7,9,5,7,8,9,10,4]
   c=0.0

   !$omp parallel 

   do i=1,M
      low = a(i)
      high = b(i)

      if (low > high) then
         write(*,*) "Exiting ",i
         exit
      endif

      !$omp do private(j)
      do j=low,high
         c(i) = c(i) - real(a(j))/real(b(j))
      enddo
      !$omp end do

   enddo
   !$omp end parallel

   do i=1,M
      write(*,'(a,i4,a,g12.6)') "i=",i,"  c(i)=",c(i)
   enddo

end program

