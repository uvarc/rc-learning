program strings
implicit none
character(LEN=:), allocatable :: str
integer :: n

   n = 11
   allocate(character(LEN=n) :: str)
   str="Hello world"
   print *, str

end program
