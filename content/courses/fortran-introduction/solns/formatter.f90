program formatter

real, dimension(10)     :: A
real, dimension(3)      :: B
integer                 :: i,m,dlen
character(len=32)       :: formatstr, pattern, chunk
character(len=1)        :: delimiter
integer,           allocatable, dimension(:) :: nvars
character(len=4),  allocatable, dimension(:) :: cn

   A=[(real(i**2),i=1,10)]
   B=[(real(j/2.),j=20,26,3)]
   m=11

   delimiter='#'
   pattern='(#e15.3,#i6,#f15.7)'

   numchars=len(pattern)
   dlen=len(delimiter)

   allocate(nvars(3))
   nvars(1)=size(A)
   nvars(2)=2
   nvars(3)=size(B)

   allocate(cn(size(nvars)))
   do i=1,len(cn)
      write(cn(i),'(i4)') nvars(i)
   enddo

   formatstr=''
   chunk=''
   n=1
   do 
      m=index(pattern,delimiter)
      if ( m==0 .or. len(pattern)==0 ) then
         exit
      else
         chunk=pattern(:m-1)
         pattern=pattern(m+dlen:)
         if (nvars(n) /= 0) then 
            formatstr=trim(adjustl(formatstr))//trim(adjustl(chunk))//&
                      trim(adjustl(cn(n)))
         else
            continue
         endif
         n=n+1
      endif
   enddo
   formatstr=trim(adjustl(formatstr))//trim(adjustl(pattern(:)))
   deallocate(nvars,cn)
   print *, formatstr

end program
