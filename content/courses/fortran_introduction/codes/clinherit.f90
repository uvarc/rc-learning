MODULE animal_class
   IMPLICIT NONE
   TYPE animal
      CHARACTER(LEN=7) :: my_name, species
      CONTAINS
         PROCEDURE     :: init=>init_animal
   END TYPE

   CONTAINS

      SUBROUTINE init_animal(self,my_name,species)
         CLASS(animal),     INTENT(INOUT) :: self
         CHARACTER(LEN=7), INTENT(IN)    :: my_name, species
         self%my_name=my_name
         self%species=species
      END SUBROUTINE
END MODULE

MODULE mammal_class
   USE animal_class
   IMPLICIT NONE
   TYPE, EXTENDS (animal) :: mammal
      CHARACTER(LEN=5) :: sound
      CONTAINS
         PROCEDURE :: speak=>print_sound
   END TYPE

   CONTAINS

      SUBROUTINE print_sound(self,sound)
         CLASS(mammal),    INTENT(INOUT) :: self
         CHARACTER(LEN=5), INTENT(IN)    :: sound

              self%sound=sound
              write(*,*) trim(self%my_name)," is a ",TRIM(self%species),       &
                         " and says ",trim(self%sound)//"."
      END SUBROUTINE
END MODULE

PROGRAM zoo
USE mammal_class
IMPLICIT NONE

   TYPE(mammal)    , DIMENSION(3) :: zoo_list
   CHARACTER(LEN=7), DIMENSION(3) :: names, species
   CHARACTER(LEN=5), DIMENSION(3) :: sounds
   INTEGER                         :: i

   names=["Raja   ","Leo    ","Bruno  "]
   species=["tiger  ","lion   ","bear   "]
   sounds=["chuff","roar ","growl"]

   DO i=1,SIZE(names)
      call zoo_list(i)%init(names(i),species(i))
   ENDDO

   DO i=1,SIZE(names)
      call zoo_list(i)%speak(sounds(i))
   ENDDO

END PROGRAM
