program formatter

real, dimension(10)     :: A
real, dimension(3)      :: B
integer                 :: i,m,n,counter,numchars
integer                 :: n1,n2,n3,n4,n5
character(len=32)       :: formatstr, pattern
character               :: c,delimiter
character(len=4),  allocatable, dimension(:) :: cn
character(len=32) :: chunk
integer,           allocatable, dimension(:) :: locs

   A=[(real(i**2),i=1,10)]
   B=[(real(j/2.),j=20,26,3)]
   m=11
   n=4

   delimiter='#'
   pattern='(#e15.3,#i6,#f15.7)'

   numchars=len(pattern)

   n1=size(A)
   n2=2
   n3=size(B)

   allocate(cn(3))
   write(cn(1),'(i4)') n1
   write(cn(2),'(i4)') n2
   write(cn(3),'(i4)') n3

   formatstr=''
   n=1
   do 
      m=index(pattern,delimiter)
      if ( m==0 .or. len(pattern)==0 ) then
         exit
      else
         chunk=pattern(:m)
         pattern=pattern(m+1:)
         if (cn(n) == '0') continue
         formatstr=formatstr//chunk//cn(n)
         n=n+1
      endif
      print *, chunk
   enddo
   print *, n,m,formatstr
   deallocate(cn)

end program
