program antidep

    integer, parameter    :: N=48
    integer, dimension(N) :: a
    integer               :: i

    do i=1,N
       a(i)=100-i
    enddo

    !$omp parallel do
    do i=1, N-1
        a(i)=a(i+1)+10
    enddo
    !$omp end parallel do

    do i=1, N
       write(*,*) i,a(i)
    enddo

end program
