program omp_par
use omp_lib

   integer :: tid

   !$omp parallel private(tid)
   tid=omp_get_thread_num()
   write(*,'(a,i4)') "Hello from thread ",tid
   !$omp end parallel

end program
