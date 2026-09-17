program pie
use omp_lib
implicit none

double precision :: area, pi, x
integer          :: i, n
integer          :: nthreads

   n=10000
   area=0.0

!$omp parallel do private(x) 
    do i=1,n
       x=(i+0.5d0)/n
       !! !$omp critical 
       area=area+4.0/(1.0d0+x**2)
       !! !$omp end critical 
    enddo
!$omp end parallel do

    pi=area/n
    write(*,'(a,f9.6)') "Pi is ", pi

end program
