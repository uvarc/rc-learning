program twod
  integer :: i, j;
  integer :: nrl, ncl, nrows, ncols
  double precision, allocatable, dimension(:,:) :: u
  double precision, allocatable, dimension(:)   :: buf

  nrl=4;
  ncl=4;

  nrows=nrl+2;
  ncols=ncl+2;
  allocate(u(0:nrows-1,0:ncols-1))
  allocate(buf(0:ncols-1))

  do i=0,nrows-1
     do j=0,ncols-1
        u(i,j)=i+j*2
     enddo
  enddo

  do i=0,nrows-1
     buf(i)=u(0,mod(i,nrows))
  enddo

  !Make sure it's right
  do i=0,nrows-1
     do j=0,ncols-1
        write(*,'(f0.1,a)',advance='no') u(i,j)," "
     enddo
     write(*,*)
  enddo

  write(*,*) "Buffer"
  do i=0,nrows-1
     write(*,'(f0.1,a)',advance='no') buf(i)," "
  enddo
  write(*,*)

end program


