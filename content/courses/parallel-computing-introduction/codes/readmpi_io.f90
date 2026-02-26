program mpiread

implicit none

integer :: N, M
integer :: i,j

integer, allocatable, dimension(:,:) :: u
character(len=64) :: filename

   if (command_argument_count() < 1) then
       stop 'USAGE: filename'
   else 
       call get_command_argument(1, filename)
   endif

   open(2, file=trim(filename), form='unformatted', access='stream')
   read(2) N,M

   allocate(u(N,M))
   read(2) ((u(i,j), i=1,N), j=1,M)
   close(2)

   write(*,*) 'N, M', N, M

   do i=1,N
      write(*,'(*(i4))') u(i,:)
   enddo

end program 

