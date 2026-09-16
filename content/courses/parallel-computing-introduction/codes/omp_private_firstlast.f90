program privatevar
use omp_lib

   integer ::  tid
   integer ::  nthreads=4
   integer ::  w,x,y,z

   x=10

   !$omp parallel do private(x) num_threads(nthreads)
   do i=1,nthreads
      tid=omp_get_thread_num()
      x=1000*(tid+1)
      write(*,'(a,i3,a,i6)') "Thread ",tid," gets x=",x
   enddo
   !$omp end parallel do
    
   write(*,'(a,i6)') "Outside parallel region x= ",x

   y=20;
   !$omp parallel do firstprivate(y) private(w) num_threads(nthreads)
   do i=1,nthreads
      tid=omp_get_thread_num()
      w=y*100*(tid+1)
      write(*,'(a,i3,a,i6)') "Thread ",tid," gets w=",w
   enddo
   !$omp end parallel do
    
   write(*,'(a,i6)') "Outside parallel region y= ",y

   z=30
   !$omp parallel do lastprivate(z) num_threads(nthreads)
   do i=1,nthreads
      tid=omp_get_thread_num()
      z=3000*(tid+1);
      write(*,'(a,i3,a,i6)') "Thread ",tid," gets z=",z
   enddo
   !$omp end parallel do
    
   write(*,'(a,i6)') "Outside parallel region z= ",z

end program
