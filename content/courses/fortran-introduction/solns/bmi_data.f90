program bmi_data
use bmi_calculator
use stats

! This program accepts data on weight, height, and body fat, computes the
!   corresponding BMIs, tabulates the BMI, and outputs the data 
!   (excluding outliers).

   real, dimension(:), allocatable     :: percent_bf, age, weight, height
   real, dimension(:), allocatable     :: cleaned_bf, cleaned_bmi
   real, dimension(:), allocatable     :: wt_kg, ht_m, bmi_values
   logical, dimension(:), allocatable  :: valid
   integer                                 :: nargs, inunit
   integer                                 :: nlines, nobs
   logical                                 :: file_exists
   character(len=40)                       :: infile
   interface
      function get_unit() result(unit_number)
      implicit none
         integer                           :: unit_number
      end function get_unit

      function line_count(unit_number)
      implicit none
         integer                           :: line_count
         integer, intent(in)               :: unit_number
      end function line_count

      subroutine convert_imp_metr(nobs,height,weight,ht_m,wt_kg)
      use bmi_calculator
      implicit none
         integer,                   intent(in)    :: nobs
         real, dimension(nobs), intent(in)    :: height, weight
         real, dimension(nobs), intent(inout) :: ht_m,   wt_kg
      end subroutine convert_imp_metr
   end interface

!Read file name from the command line
   nargs=command_argument_count()
   if (nargs .ne. 1) then
      stop "No data file specified"
   else
      call get_command_argument(1,infile)
   endif

   inquire(file=infile,exist=file_exists)

   if (file_exists) then
      inunit=get_unit()
      open(inunit,file=infile,iostat=ios)
      if ( ios .ne. 0 ) then
         stop "Unable to open data file"
      endif
   else
      stop "Data file not found"
   endif

   nlines=line_count(inunit)

   ! Subtract header line
   nobs=nlines-1

   allocate(percent_bf(nobs), age(nobs), weight(nobs), height(nobs))
   allocate(bmi_values(nobs), wt_kg(nobs),   ht_m(nobs))
   allocate(valid(nobs))

   !Read header
   read(inunit,*)

   !Read data 
   do n=1,nobs
      read(inunit,*,iostat=ios) percent_bf(n),age(n),weight(n),height(n)
      if ( ios .ne. 0 ) stop "Error reading file"
   enddo

   !Convert units
   call convert_imp_metr(nobs,height,weight,ht_m,wt_kg)

   !Compute BMI array
   bmi_values=BMI(ht_m,wt_kg)

   !Reject outliers
   valid=reject_outliers(bmi_values)
   nvalid=count(valid)
   allocate(cleaned_bf(nvalid),cleaned_bmi(nvalid))
   cleaned_bf =pack(percent_bf,valid)
   cleaned_bmi=pack(bmi_values,valid)

   !Write data to CSV file for plotting
   call dump_results(infile,nvalid,cleaned_bf,cleaned_bmi)

end program bmi_data

function get_unit() result(unit_number)
implicit none
   integer               :: unit_number
   integer               :: base_unit=10
   logical               :: first=.true.

   if ( first ) then
       unit_number=base_unit
       first=.false.
   else
       unit_number=unit_number+1
   endif
end function get_unit

function line_count(inunit)
implicit none
   integer             :: line_count
   integer, intent(in) :: inunit
   integer             :: nlines
   nlines=0
   do
      read(inunit,*,end=10)
      nlines=nlines+1
   enddo
10 continue

   rewind(inunit)
   line_count=nlines
end function line_count

subroutine convert_imp_metr(nobs,height,weight,ht_m,wt_kg)
use bmi_calculator
implicit none
   integer,               intent(in)    :: nobs
   real, dimension(nobs), intent(in)    :: height, weight
   real, dimension(nobs), intent(inout) :: ht_m,   wt_kg
   integer                              :: i

   do i=1,nobs
      call imperial_to_metric(height(i),weight(i),ht_m(i),wt_kg(i))
   enddo
   

end subroutine convert_imp_metr

subroutine dump_results(infile,nobs,percent_bf,bmi_values)
implicit none
   integer,               intent(in)  :: nobs
   real, dimension(nobs), intent(in)  :: percent_bf, bmi_values
   integer                            :: period
   integer                            :: outunit, ios
   integer                            :: n
   character(len=40)                  :: infile,outfile
   interface
      function get_unit() result(unit_number)
      implicit none
         integer                           :: unit_number
      end function get_unit
   end interface
   !Prepare file to plot with plotting software.
   !Using the same name as infile but a different suffix.
   outunit=get_unit()
   period=index(infile,'.')
   if ( period > 0 ) then
      outfile=infile(1:period-1)//"_plt.csv"
   else
      outfile=infile//"_plt.csv"
   endif
   open(outunit,file=outfile,iostat=ios)
   if ( ios .ne. 0 ) then
      stop "Unable to open output file"
   endif

   do n=1,nobs
      write(outunit,'(f8.2,a,f8.2)') percent_bf(n),",",bmi_values(n)
   enddo
end subroutine dump_results
