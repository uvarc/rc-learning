module bird_dat
implicit none

type bird_data
   character(len=50)                  :: species
   integer, dimension(:), allocatable :: obs

   contains
       procedure:: init=>constructor
       procedure:: stats
       procedure:: minmax
end type bird_data

private constructor, stats, minmax

contains
   
subroutine constructor(bird,species,dat,n)
      class(bird_data),        intent(inout)          :: bird
      character(len=50),       intent(in)             :: species
      integer,                 intent(in)             :: n
      integer, dimension(n),   intent(in)             :: dat

      bird%species=species
      allocate(bird%obs(n))
      bird%obs=dat
end subroutine

subroutine stats(bird,mean,std)
      class(bird_data),        intent(inout)          :: bird
      real,                    intent(out)            :: mean,std
      real                                            :: mu2
      
      mean=sum(bird%obs)/float(size(bird%obs))
      mu2=(sum(bird%obs)/float(size(bird%obs)))**2
      std=sqrt(sum(bird%obs**2-(mu2))/float(size(bird%obs)))
end subroutine

subroutine minmax(bird,years,min_val,max_val,min_year,max_year)
      class(bird_data),      intent(inout) :: bird
      integer, dimension(:), intent(in)    :: years
      real,                  intent(out)   :: min_val,max_val
      integer,               intent(out)   :: min_year, max_year
      
      min_val=minval(bird%obs)
      max_val=maxval(bird%obs)
      min_year=years(minloc(bird%obs,1))
      max_year=years(maxloc(bird%obs,1))

end subroutine

end module
