module bmi_calculator
implicit none

!This module computes BMI from height and weight.
!Units are converted from metric (kg, cm) or Imperial (ft/in, lb) to
!to (kg,m). 

!Globals

   integer, parameter               :: rk=kind(1.0)
   real(rk)                         :: invalid= 999._rk
   real(rk)                         :: signal =-999._rk

   contains

!--------------------------------------------------------------------------
!

   elemental function cm_to_m(l)
      real(rk)             :: cm_to_m
      real(rk), intent(in) :: l
         cm_to_m=0.01*l
   end function cm_to_m
      
   elemental function inch_to_m(l)
      real(rk)             :: inch_to_m
      real(rk), intent(in) :: l
      real(rk), parameter  :: inch2m=0.0254_rk
         inch_to_m=inch2m*l
   end function inch_to_m
      
   elemental function lb_to_kg(l)
      real(rk)             :: lb_to_kg
      real(rk), intent(in) :: l
      real(rk), parameter  :: lb2kg=0.45359237_rk
         lb_to_kg=lb2kg*l
   end function lb_to_kg

   subroutine imperial_to_metric(l_inch,wt_lb,l_m,wt_kg)
      real(rk), intent(in) :: l_inch, wt_lb
      real(rk), intent(out):: l_m, wt_kg
         l_m   = inch_to_m(l_inch)
         wt_kg = lb_to_kg(wt_lb)
   end subroutine imperial_to_metric

   elemental function BMI(ht,wt)
      real(rk)             :: BMI
      real(rk), intent(in) :: ht,wt
         BMI=wt/ht**2
   end function BMI

   function BMI_table(bmi_val)
      integer          :: BMI_table
      real(rk), intent(in) :: bmi_val

      if (bmi_val <  16.0 )                      BMI_table=1
      if ( bmi_val>= 16.0 .and. bmi_val < 18.5 ) BMI_table=2
      if ( bmi_val>= 18.5 .and. bmi_val < 25.0 ) BMI_table=3
      if ( bmi_val>= 25.0 .and. bmi_val < 30.0 ) BMI_table=4
      if ( bmi_val>= 30.0 .and. bmi_val < 35.0 ) BMI_table=5
      if ( bmi_val>= 35.0 .and. bmi_val < 40.0 ) BMI_table=6
      if (bmi_val >= 40.0 )                      BMI_table=7
   end function BMI_table

   function is_inrange(inval,bounds)
      logical                       :: is_inrange
      real(rk),              intent(in) :: inval
      real(rk),dimension(2), intent(in) :: bounds
         if ( bounds(1) <= inval .and. inval <= bounds(2) ) then
            is_inrange=.true.
         else
            is_inrange=.false.
         endif
   end function is_inrange

end module bmi_calculator
