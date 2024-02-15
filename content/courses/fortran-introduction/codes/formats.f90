program printit
   implicit none

   integer, parameter :: rk=kind(1.0)
   integer, parameter :: dk=kind(1.d0)

   complex  :: z
   real(rk) :: rpi=4.0*atan(1.0)
   real(rk) :: dpi=4.d0*atan(1.d0)
   real(rk) :: res
   integer  :: i,j
   integer  :: i1=42, i2=99
   logical  :: is_zero, is_finite
   character(len=12) :: mess1, mess2

   real(rk), dimension(4)   :: b
   real(rk), dimension(4,4) :: arr

   res=cos(rpi)

   do i=1,size(arr,1)
      b(i)=real(i)
      do j=1,size(arr,2)
          arr(i,j)=real(i+j)
      enddo
   enddo

   z=cmplx(res,rpi)

   mess1="First result"
   mess2="An integer"

   is_zero=.false.
   is_finite=.true.

   write(*,'(i5,2x,i6)') i1,i2
   write(*,'(i5,a,i6)') i1,"     ",i2
   write(*,'(a,f0.6)') "The result is  ",res
   write(*,'(a,i4,es15.7)') "The answer",i1,dpi
   write(*,'(4(i2,3x,f8.3))') (j,b(j),j=1,4)
   write(*,'(2f8.2)') z !complex
   write(*,'(2L)')is_zero,is_finite
   write(*,'(2p,f8.2,0p,f8.2)') rpi, dpi
   write(*,'(a,f8.2,/,a,i6)') mess1,res,mess2,i1
   write(*,'(a)') ' '
   write(*,'("first value ",f8.2,", second value ",i6)') res
   write(*,'(a)') ' '
   write(*,'("first value ",f8.2,:," second value ",i6)') res

   write(*,*) '----------------------'

   do i=1,size(arr,1)
      write(*,'(*(g0))') arr(i,:)
   enddo

   write(*,*) '----------------------'

   do i=1,size(arr,1)
      write(*,'(*(f12.4))') arr(i,:)
   enddo

! Old style
   WRITE ( *, 2 ) i1
   WRITE ( *, 2 ) i1,i2
2  FORMAT ( 1X 'RESULT = ', I2, :, 3X, 'AT INDEX = ', I2 )

   write(*,*) 

   write(*,'(f8.2)',advance='no') res
   write(*,'(i6)',advance='yes') i2
   write(*,*) 'All done'

end program
