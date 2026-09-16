program omp_hello
use omp_lib
implicit none

   integer :: i
   integer :: nthreads, tid

   !$omp parallel private(tid)
      nthreads=omp_get_num_threads()
      tid=omp_get_thread_num()
      write(*,'(a,i3,a,i3)') "Hello from thread ",tid," of ",nthreads
   !$omp end parallel

end program
