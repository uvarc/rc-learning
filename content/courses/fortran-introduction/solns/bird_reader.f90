program bird_obs
use bird_dat
implicit none

   type(bird_data),dimension (:), allocatable :: bird_list
   character(len=50)                          :: filename
   character(len=50)                          :: species,my_species,lc_species
   integer                                    :: l, n, nargs, nbirds, nobs
   integer, dimension(:),allocatable          :: years

   interface
      subroutine read_data(bird_list,filename,years)
         use bird_dat
         implicit none
         type(bird_data), dimension(:), allocatable, intent(out) :: bird_list
         character(len=*),                           intent(in)  :: filename
         integer,         dimension(:), allocatable, intent(out) :: years
      end subroutine
   end interface

   nargs=command_argument_count() 
   if ( nargs .ne. 1 ) then
      stop "Usage: <file>"
   else
      call get_command_argument(1,filename)
   endif

   call read_data(bird_list,filename,years)
   nbirds=size(bird_list)
   nobs  =size(bird_list(1)%obs)

   write(*,*) "Observations over the years ",years
   do n=1,nbirds
       write(*,'(a)',advance='no') trim(bird_list(n)%species)
       do l=1,nobs-1
          write(*,'(i4,a)',advance='no') bird_list(n)%obs(l),","
       enddo
       write(*,'(i4)') bird_list(n)%obs(nobs)
   enddo

End program 

subroutine read_data(bird_list,filename,years)
   use bird_dat
   implicit none
   type(bird_data), dimension(:), allocatable, intent(out) :: bird_list
   character(len=*),                           intent(in)  :: filename
   integer,         dimension(:), allocatable, intent(out) :: years
   integer,         dimension(:), allocatable :: obs
   integer,         parameter                 :: nobs=10
   character(len=6),dimension(nobs)           :: cyears
   integer                                    :: inunit
   integer                                    :: nheaders,nlines,nbirds
   character(len=50)                          :: species
   character(len=1024)                        :: line
   character(len=:),dimension(:),allocatable  :: line_vals
   integer                                    :: num_vals
   integer                                    :: n,m
   logical                                    :: file_exists

   inquire(file=filename,exist=file_exists)
   if (file_exists) then
      inunit=10
      open(10,file=filename)
   else
      stop "Can't find file."
   endif

   nlines=0
   do
     read(inunit,*,end=10)
     nlines=nlines+1
   enddo
10 continue
   rewind(inunit)

   nheaders=1
   nbirds=nlines-nheaders
   allocate(bird_list(nbirds))
   allocate(years(nobs))
   allocate(obs(nobs))
     
   do m=1,nheaders
      read(inunit,*) species,years
   enddo
   do n=1,nbirds
      read(inunit,*) species, obs
      call constructor(bird_list(n),species,obs)
   end do

end subroutine read_data
