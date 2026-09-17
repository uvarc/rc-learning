program omp_hello
use omp_lib
implicit none

   integer :: i, N, nthreads
   integer :: tid

   N=40
   nthreads=4

   !$omp parallel do num_threads(nthreads)
   do i=1,N
      tid=omp_get_thread_num()
      write(*,'(a,i3,a,i3)') "Thread ",tid," runs i= ",i
   enddo
   !$omp end parallel do

end program
