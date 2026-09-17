program truedep

    integer, parameter    :: N=48
    integer, dimension(N) :: a
    integer               :: x=5
    integer               :: i

    a(1)=100

    !$omp parallel do
    do i=1, N-1
        a(i+1)=a(i)+10
    enddo
    !$omp end parallel do

    do i=1, N
       write(*,*) i,a(i)
    enddo

end program
