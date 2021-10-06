program bird_obs
use bird_dat
use sorters
implicit none
!******************************************************************
!Mean observations, standard deviation, max and min of a set of 
!bird observations 
!Author:    Katherine Holcomb
!Changelog: Initial version 2015-03-4		
!******************************************************************

  class(bird_data),dimension (:), allocatable :: bird_list
  real,            dimension(:),  allocatable :: means,stds,oldmeans
  character(len=50)                           :: filename
  character(len=50)                           :: species,my_species,lc_species
  integer                                     :: n, nargs, nbirds
  logical                                     :: found_it
  real                                        :: std,mean,min_val,max_val
  integer                                     :: min_year, max_year
  integer, dimension(:),allocatable           :: years, pvec

  interface
     subroutine read_data(bird_list,filename,years)
        use bird_dat
        use file_utils
        implicit none
        class(bird_data), dimension(:),allocatable, intent(out) :: bird_list
        character(len=*),                           intent(in)  :: filename
        integer,         dimension(:),allocatable,  intent(out) :: years
     end subroutine
  end interface

  nargs=command_argument_count() 
  if ( nargs .ne. 2 ) then
     stop "Usage: <file> species"
  else
     call get_command_argument(1,filename)
     call get_command_argument(2,my_species)
  endif

  call read_data(bird_list,filename,years)
  nbirds=size(bird_list)

  found_it=.false.
  allocate(means(nbirds),stds(nbirds))
  do n=1,nbirds
     call bird_list(n)%stats(means(n),stds(n))
     if (my_species==bird_list(n)%species) then
        mean=means(n)
        std=stds(n)
        call bird_list(n)%minmax(years,min_val,max_val,min_year,max_year)
        found_it=.true.
     endif
  enddo

  if (found_it) then
     write(*,*) "Statistics for "//my_species
     write(*,'(a,f8.2)',advance='no') "The minimum value of observations is:",min_val
     write(*,'(a,i5)') " in year",min_year
     write(*,'(a,f8.2)',advance='no') "The maximum value of observations is:",max_val
     write(*,'(a,i5)') " in year",max_year
     write(*,'(a,f0.2)') "The mean value of the observations is:",mean
     write(*,'(a,f0.2)') "The standard deviation of the observations is:",std
  else
     write(*,*) "Requested species not found in the data file."
  endif

  allocate(pvec(nbirds))
  allocate(oldmeans(size(means)))
  oldmeans=means
  call pshellsort(means,pvec,nbirds)

  write(*,*)
  write(*,*) 'The 10 most common birds are'

  do n=nbirds,nbirds-9,-1
      write(*,'(a,f8.2)') bird_list(pvec(n))%species(1:len_trim(species)),oldmeans(pvec(n))
  enddo

End program 

subroutine read_data(bird_list,filename,years)
  use bird_dat
  use file_utils
  implicit none
  class(bird_data),dimension(:), allocatable, intent(out) :: bird_list
  character(len=*),                           intent(in)  :: filename
  integer,         dimension(:), allocatable, intent(out) :: years
  integer,         dimension(:), allocatable :: obs
  integer,         parameter                 :: nobs=47
  character(len=6),dimension(nobs)           :: cyears
  integer                                    :: inunit
  integer                                    :: nheaders,nbirds
  character(len=50)                          :: species
  character(len=1024)                        :: line
  character(len=:),dimension(:),allocatable  :: line_vals
  integer                                    :: num_vals
  integer                                    :: n

  inunit=open_file(filename,'r')

  if (inunit .ne. 0) then
      open(unit=inunit,file=filename)
  else
      stop "Unable to open specified data file."
  endif

  nheaders=3
  nbirds=count_records(inunit,nheaders)

  allocate(bird_list(nbirds))
  allocate(years(nobs))
  read(inunit,*) species,cyears(:)
  do n=1,nobs
     read(cyears(n),'(i4)') years(n)
  enddo

  allocate(obs(nobs))
     
  do n=1,nbirds
     read(inunit,*) species, obs
     call bird_list(n)%init(species,obs,nobs)
  end do

end subroutine read_data
