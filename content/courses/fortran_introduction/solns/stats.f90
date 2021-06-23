module stats
implicit none
!This module contains some simple functions to work with imperfect data
!  Author:    Katherine Holcomb
!  Changelog: Initial code 20120220
!             Minor improvements 20160307

   integer, parameter  :: rk=kind(1.0)
   real(rk), parameter :: MISSING=-999._rk

   contains

      function mean(A)
      ! Computes the mean of an array A
         real(rk)                           :: mean
         real(rk), intent(in), dimension(:) :: A
         integer                            :: lenA

         lenA=size(A)
         mean=sum(A)/size(A)

      end function mean

      function std(A)
      ! Computes the standard deviation of an array A
         real(rk)                           :: std
         real(rk), intent(in), dimension(:) :: A
         integer                            :: lenA

         lenA=size(A)
         std =sqrt((sum(A**2)/lenA)-mean(A)**2)

      end function std

      function reject_outliers(A)  result(valid)
      !Returns the indices of A with true for valid, false for outlier
      ! A must be inout if it is passed to something else as an argument 
      ! (since the compiler doesn't try to trace the whole call tree to 
      ! figure out whether it's changed or not).
         real(rk),    dimension(:),       intent(inout) :: A
         logical,     dimension(size(A))                :: valid
         real(rk),    dimension(size(A))                :: devs, prob
         real(rk)                                       :: mean_A, stdv_A
         real(rk)                                       :: criterion
         integer                                        :: lenA

         mean_A=mean(A)
         stdv_A=std(A)
         lenA = size(A)
         criterion = 1.0_rk/(2*lenA)     ! Chauvenet's criterion
         devs=abs(A-mean_A)/stdv_A
         devs = devs/sqrt(2.0_rk)        ! The left and right tail threshold values
         prob = erfc(devs)               
         valid=prob>=criterion

      end function reject_outliers

      subroutine fix_missing(A,n_bad)
      !Returns A with actual data substituted for missing data
      !Relying on a side effect here, hence we use a subroutine.
         real,    dimension(:),       intent(inout) :: A
         integer,                     intent(out)   :: n_bad
         integer                                    :: lenA
         integer                                    :: i

         lenA=size(A)
         n_bad=0
         do i=1,lenA
            if (A(i)==MISSING) then
               n_bad=n_bad+1
               if (i==0) then 
                  A(i)=A(i+1)
               else
                  A(i)=A(i-1)
               endif
            endif
         enddo

      end subroutine fix_missing

end module stats
