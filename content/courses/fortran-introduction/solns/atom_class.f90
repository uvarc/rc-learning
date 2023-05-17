module Atoms

   type :: Atom
      character(len=2) :: symbol
      character(len=20):: name
      real             :: isotopic_mass
      integer          :: atomic_number
         contains
            procedure  :: neutron_number
   end type

   contains

      integer function neutron_number(self)
         class(Atom), intent(in) :: self

         neutron_number=floor(self%isotopic_mass)-self%atomic_number
      end function

end module




