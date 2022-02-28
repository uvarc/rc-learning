PROGRAM  myprog
   INTEGER  :: i,N
   REAL     :: x_start,x_end,incr,x,y
   REAL     :: func


   x_start=-4.0
   x_end  = 4.0
   N      = 401
   incr   =(x_end-x_start)/(N+1)

   open(10,file="function.csv")

   x=x_start
   do i=1,N
      y=func(x)
      write(10,'(f15.8,a,f15.8)') x,",",y
      x=x+incr
   enddo

END PROGRAM

REAL FUNCTION func(x)
   REAL, INTENT(IN) :: x
   REAL             :: pi=4.0*atan(1.0)

   func=1./(pi*(1.+x**2))

END FUNCTION
