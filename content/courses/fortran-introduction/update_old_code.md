---
title: "Updating Old Code"
toc: true
type: book
weight: 100

menu:
    fortran-introduction:
        parent: Updating Old Code
        weight: 100

---

If your dissertation depends on it (and you are allowed to do it) it's worth the time unless the code is more than 50,000 to 100,000 lines or so.

Step 1: Replace all COMMON blocks with modules.  Initially these modules only need to declare the variables.

Step 2: Reorganize subprograms into modules.  This gives you a free interface, and checks agreement of type and number of arguments.

Step 3: Change variable names to something more meaningful as you are able.

Step 4: Globals are poison, and a major source of bugs. 
The author found a bug in a model that was due to a variable being global due to having been originally in COMMON, and ported to a MODULE. This caused the program to have a "memory" when it should not have had one, but since the users had never executed the model for different conditions in the same run, they had never noticed this error. 
Move variables out of the "common" modules into parameter lists as quickly as you can.

Step 5: Introduce types/classes as appropriate.

## Testing

You will need a set of _regression tests_ for this job.  A regression occurs when a change to a code creates an error in previously-tested scenarios.  Before starting to modify code, determine how you will test it as it stands.
You may find the original code was not as well tested as you expected (or hoped).  You may need to create a suite of tests.  Retain those and run them as you make modifications.

## Programming Style

Style makes a big difference, especially in compiled languages that tend to be more verbose than interpreted languages.
Indentation is very important
Variable names are important–choose variable names that communicate some meaning to the reader.  Variable names are no longer limited to 8 characters.
We do not use green-and-white striped fanfold paper anymore to separate lines visually.  
Insert blank lines between logical chunks of code

**Example**

**Before**
The obsolete DATA statement was used to initialize variables.  The old code below is written in Fortran 77, so still used fixed format, but some newer constructs such as END DO were available.
```fortran
subroutine biogeoclimate
include "common.txt"
real tx(365),tn(365),ta(365),p(365)
real t1(12),t2(12),p1(12)
real littc1,littc2,littn1,littn2
real prcp_n
data prcp_n/0.00002/

c prcp_n: Nitrogen content for unit rianfall (tN/ha)
c
  rain_1=0.0
  rain_n=0.0
  do i=1,12
    tmp1=ynormal(0.0,1.0)
    tmp1=max(-1.0,min(tmp1,1.0))
    t1(i)=tmin(i)+tmp1*tminv(i)
    t2(i)=tmax(i)+tmp1*tmaxv(i)
    tmp2=ynormal(0.0,1.0)
    tmp2=max(-0.5,min(tmp2,0.5))
c   forest cover can increase rainfall by maximum 15%
    p1(i)=max(prec(i)+tmp2*precv(i),0.0)
c         *(1.0+lai*0.02)
    rain_1=rain_1+p1(i)
c   write(*,*) tmp1, tmp2, tmin(i),tmax(i),prec(i)
    rain_n=rain_n+p1(i)*prcp_n
  end do

  call cov365(t1,tn)
  call cov365(t2,tx)
  call cov365a(p1,p)

  do i=1,365
  ta(i)=0.5*(tn(i)+tx(i))
  end do
c
c Daily cycles of C, N, H2O
c
  rrr=0.0
  ypet=0.0
  yaet=0.0
  avail_n=0.0
  degd=0.0
  growdays=0.0
  drydays=0.0
  drydays1=0.0
  flooddays=0.0
c
  aoc0=aoc0+go_ao_c
  aon0=aon0+go_ao_n
```

**After**
```fortran
module Model
use Parameters
use Constants
use Soil
use Site
use Species
use Tree
use Random
use Climate
use Input
implicit none
   real, parameter                          :: grow_min=0.05
   real, parameter                          :: growth_thresh=0.05
   real                                     :: growth_min=0.01

   contains

   subroutine BioGeoClimate(site,year)
      integer,         intent(in)    :: year
      type(SiteData),  intent(inout) :: site
      integer                        :: gcm_year
      integer                        :: num_species
      real, dimension(NTEMPS)        :: tmin, tmax, prcp
      real, dimension(NTEMPS)        :: tmptmin, tmptmax, tmpprec
      real, dimension(days_per_year) :: daytemp, daytemp_min, daytemp_max
      real, dimension(days_per_year) :: daynums, dayprecip
      real                   :: litter_c_lev1,litter_c_lev2
      real                   :: litter_n_lev1,litter_n_lev2
      real                   :: rain,rain_n,freeze
      real                   :: temp_f,prcp_f
      real                   :: total_rsp,avail_n,n_avail,C_resp,pet,aet
      real                   :: growdays,drydays_upper,drydays_base
      real                   :: flooddays,degday
      real                   :: outwater
      real                   :: exrad,daylength,exradmx
      real                   :: pot_ev_day
      real                   :: act_ev_day
      real                   :: laiw0_ScaledByMax,laiw0_ScaledByMin
      real                   :: aow0_ScaledByMax,aow0_ScaledByMin
      real                   :: sbw0_ScaledByMax,sbw0_ScaledByMin
      real                  :: saw0_ScaledByFC,saw0_ScaledByWP
      real                   :: yxd3   !used but never set
      ! used to temporarily hold accumulated climate variables
      real                   :: tmpstep1,tmpstep2
      real                   :: tmp
      integer                :: i,j,k,m
      real, parameter        :: min_grow_temp =5.0
      real, parameter        :: max_dry_parm  =1.0001
      real, parameter        :: min_flood_parm=0.9999

      save

      num_species=size(site%species)
      rain  =0.0
      rain_n=0.0

     ! The user is expected to input decr_by values as positive.
     if ( linear_cc ) then
        if (year .ge.begin_change_year.AND. year .le.                         &
           (begin_change_year+duration_of_change)) then
                accumulated_tmin=accumulated_tmin+tmin_change
                accumulated_tmax=accumulated_tmax+tmax_change

            do m=1,12
               tmpstep1 =site%precip(m) +accumulated_precip(m)
               tmpstep2 = tmpstep1 *precip_change
               accumulated_precip(m) =accumulated_precip(m) + tmpstep2
            end do

        endif

     else if (use_gcm) then
        gcm_year=start_gcm+year-begin_change_year
        if (gcm_year.ge.start_gcm.and.gcm_year.le.end_gcm) then
           call read_gcm_climate(site%site_id,gcm_year,start_gcm,tmin,tmax,prcp)
           site%tmin=tmin
           site%tmax=tmax
           site%precip=prcp*mm_to_cm
         endif
     endif
end module
```

**Changes**

1. Converted to modules.

2. Eliminated COMMON, passing all variables (this not only controls the interface, but eventually eliminated a bug).

3. Renamed most variables to something more descriptive.

4. Added lots of whitespace for visual separation.

5. Aligned typographically

Note that considerable effort went into separating other parts of the code into the modules USEd in this subroutine.
