program outdep

    integer, parameter    :: N=48
    integer, dimension(N) :: a
    integer               :: x=5
    integer               :: i

    a(N)=100;

    !$omp parallel do
    do i=1, N-1
        a(i)=i
        a(i+1)=x+i
    enddo
    !$omp end parallel do

    do i=1, N
       write(*,'(i4,x,i4)') i, a(i)
    enddo

end program
