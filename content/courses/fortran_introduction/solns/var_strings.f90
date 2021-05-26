program strings
implicit none
character(len=:), allocatable :: title, subtitle, full_title
character                     :: c
integer                       :: nchars, n

   allocate(character(len=4) :: title)
   allocate(character(len=22) :: subtitle)

   title="Jaws"
   subtitle="The Revenge"

   nchars=len(title)+len(subtitle)+1
   allocate(character(len=nchars) :: full_title)
   full_title=title//":"//subtitle
   print *, full_title
   print *, len(full_title)
   print *, full_title(2:4)

   full_title="P"//full_title(2:)
   print *, full_title

end program
