module Fractions
   implicit none
   private

   public :: Fraction

   type Fraction
      private
      integer              :: num, denom
      contains
         private
         procedure adder
         procedure subber
         procedure multer
         procedure divver
         procedure copier
         procedure, public :: reduce
         procedure, public :: print=>printer
         generic, public   :: operator(+) => adder
         generic, public   :: operator(-) => subber
         generic, public   :: operator(*) => multer
         generic, public   :: operator(/) => divver
         generic, public   :: assignment(=) => copier
   end type 

   interface Fraction
        module procedure init
   end interface

   contains

      !Helpers

      integer function gcd(num, denom)
         integer, intent(in)  :: num, denom
         integer              :: den, temp
         gcd=num
         den=denom
         do while ( den /= 0 )
            temp=den
            den=modulo(gcd,den)
            gcd=temp
         end do
      end function

      type(Fraction) function init(num,denom) 
         integer, intent(in)  :: num, denom
            if ( denom /= 0 ) then
               init%num=num
               init%denom=denom
            else
               init%num=0
               init%denom=0
            endif
      end function init

      !Methods

      subroutine reduce(self)
         class(Fraction)               :: self
         integer                       :: g_c_d
            g_c_d=gcd(self%num,self%denom)
            self%num  =self%num/g_c_d
            self%denom=self%denom/g_c_d
      end subroutine reduce
         
      type(Fraction) function adder(self,f)
         class(Fraction), intent(in)   :: self
         type(Fraction),  intent(in)   :: f
         integer                       :: num, denom 
            if ( self%denom .ne. 0 .and. f%denom .ne. 0 ) then
               num=self%num*f%denom+f%num*self%denom
               denom=self%denom*f%denom
               adder=Fraction(num,denom)
               call adder%reduce()
            else
               adder=Fraction(0,0)
            endif
      end function adder
         
      type(Fraction) function subber(self,f)
         class(Fraction), intent(in)   :: self
         type(Fraction),  intent(in)   :: f
         integer                       :: num, denom 
            if ( self%denom .ne. 0 .and. f%denom .ne. 0 ) then
               num=self%num*f%denom-f%num*self%denom
               denom=self%denom*f%denom
               subber=Fraction(num,denom)
               call subber%reduce()
            else
               subber=Fraction(0,0)
            endif
      end function subber
         
      type(Fraction) function multer(self,f)
         class(Fraction), intent(in)   :: self
         type(Fraction),  intent(in)   :: f
         integer                       :: num, denom 
            if ( self%denom .ne. 0 .and. f%denom .ne. 0 ) then
               denom=self%denom*f%denom
               num=self%num*f%num
               multer=Fraction(num,denom)
               call multer%reduce()
            else
               multer=Fraction(0,0)
            endif
      end function multer
         
      type(Fraction) function divver(self,f)
         class(Fraction), intent(in)   :: self
         type(Fraction),  intent(in)   :: f
         integer                       :: num, denom 
            if ( self%denom .ne. 0 .and. f%denom .ne. 0 ) then
               denom=self%denom*f%num
               num=self%num*f%denom
               divver=Fraction(num,denom)
               call divver%reduce()
            else
               divver=Fraction(0,0)
            endif
      end function divver

      subroutine copier(new,old)
         class(Fraction), intent(out) :: new
         class(Fraction), intent(in)  :: old
             new%num=old%num
             new%denom=old%denom
      end subroutine copier

      subroutine printer(self,iunit)
         class(Fraction), intent(in) :: self
         integer,         intent(in) :: iunit
            write(iunit,'(i0,a,i0)') self%num,'/',self%denom
     end subroutine printer

end module Fractions
