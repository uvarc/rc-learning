---
title: "Advanced Classes"
toc: true
type: book
weight: 76

menu:
    fortran_introduction:
        parent: Advanced Classes
        weight: 76
---

Classes are fairly new to Fortran and we will mention only a few more of their features.  Some, such as inheritance, are applicable to all types.  We will describe some additional features here.

## Data Hiding

One of the main purposes of OOP is to prevent outside units from doing anything without “sending a message” to an appropriate instance.
The previous example violates this principle.  We can make everything private, which means that only members of the module can access the symbols.  We must then go through an instance of the type/class to invoke the methods.
Making a type public “exposes” the type name and its type-bound procedures, but not its attributes.
We will modify the example to accomplish this.

### Modified Example

MODULE mytype_class
IMPLICIT NONE
   PRIVATE  !Everything contained is now private
   PUBLIC ::MyType!so need to make the type public

   TYPE MyType
      PRIVATE  !Methods must be declared private
      INTEGER   ::i,j
      REAL      ::x,y
      CONTAINS
         PROCEDURE ::init=>init_class
         PROCEDURE :: write=>write_class
   END TYPE

   CONTAINS

   SUBROUTINE init_class(self,stuff1,stuff2)
      CLASS(MyType), INTENT(INOUT) :: self
      REAL,          INTENT(IN)    :: stuff1, stuff2
      self%i=0; self%j=0
      self%x=stuff1; self%y=stuff2
   END SUBROUTINE

   SUBROUTINE write_class(self,iunit)
      CLASS(myType), INTENT(IN) :: self
      INTEGER, INTENT(IN)       ::iunit

      WRITE(*,*) "Integers ",self%i,self%j
      WRITE(*,*) "Reals",self%x,self%y
   end subroutinewrite_class

   SUBROUTINE reset(self,i1,i2,stuff1,stuff2)
      CLASS(MyType), INTENT(INOUT) :: self
      INTEGER,       INTENT(IN)    :: i1,i2
      REAL,          INTENT(IN)    :: stuff1, stuff2
      self%i=i1; self%j=i2
      self%x=stuff1; self%y=stuff2
   END SUBROUTINE

END MODULE
```
In the caller:
``
CALL myvar%init(x,y)
CALL write_class(myvar,11) ! illegal, link error
CALL myvar%write(12)       ! OK
```

## Constructors and Destructors

A constructor is a subprogram that handles the bookkeeping to initialize an instance of a type. This may entail:
  * Assigning values to attributes
  * Allocating memory for allocatable arrays
    * This _never_ happens automatically.  If an allocatableis a member of a type, a constructor must be written.
A destructor is a subprogram that releases memory for a type.  This may be required if you allocate in a constructor.  The garbage collection in subprograms will not release memory allocated for a type.

Fortran has no special syntax for a constructor or destructor.  Programmers can define an `init` function or equivalent, then declare it `private` to be sure it can be accessed only through a type instance.  Destructors can be similarly written to deallocate arrays.

## Inheritance

Inheritance is when a type is derived from another type and has access to its members.  Inheritance is not restricted to classes in Fortran but can be used with types as well.
```fortran
type Parenttype
integer ::my_id
real    ::my_value
end typeParenttype

type, extends (Parenttype) :: Childtype
integer ::my_int
end type Childtype
```

### Attribute Inheritance

The child type inherits all the attributes of its parent.
```fortran
type(ChildType) :: billy
billy%my_id !is valid, and is equivalent to
billy%ParentType%my_id
```
But billy%my_int does not refer back to the parent, since that variable occurs only in the extension.

### Class Inheritance

When a class is extended, not only the attribute but the methods are inherited.
{{< code-download file="/courses/fortran_introduction/codes/clinherit.f90" lang="fortran" >}}

# Exercise

* Write a class Atom that contains the following attributes:
  * Element symbol
  * Element name
  * Atomic mass
  * Atomic number
* The method should be
  * Compute and return the number of neutrons from the mass and number (n=mass-number)
* Also write a routine to initialize an instance of the class (a constructor).


