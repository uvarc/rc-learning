MODULE mytype_class
IMPLICIT NONE
   PRIVATE  !Everything contained is now private
   PUBLIC ::MyType !so need to make the type public

   TYPE MyType
      PRIVATE  !Members must still be declared private
      INTEGER   ::i,j
      REAL      ::x,y
      CONTAINS
         PROCEDURE ::init=>init_class
         PROCEDURE :: write=>write_class
   END TYPE

   CONTAINS

   SUBROUTINE init_class(self,stuff1,stuff2)
      CLASS(MyType), INTENT(INOUT) :: self
      INTEGER,       INTENT(IN)    :: i1, i2
      REAL,          INTENT(IN)    :: stuff1, stuff2
      self%i=0; self%j=0
      self%x=stuff1; self%y=stuff2
   END SUBROUTINE

   SUBROUTINE write_class(self,iunit)
      CLASS(myType), INTENT(IN) :: self
      INTEGER, INTENT(IN)       ::iunit

      WRITE(*,*) "Integers ",self%i,self%j
      WRITE(*,*) "Reals",self%x,self%y
   END SUBROUTINE

   SUBROUTINE reset(self,i1,i2,stuff1,stuff2)
      CLASS(MyType), INTENT(INOUT) :: self
      INTEGER,       INTENT(IN)    :: i1,i2
      REAL,          INTENT(IN)    :: stuff1, stuff2
      self%i=i1; self%j=i2
      self%x=stuff1; self%y=stuff2
   END SUBROUTINE

END MODULE

