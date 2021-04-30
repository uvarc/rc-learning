---
title: "Classes"
toc: true
type: docs
weight: 90

menu:
    fortran_introduction:
        name: Classes
        weight: 90
---

# Fortran classes

# OOP Terminology

* An _instance_ of a type or class is a variable of that type/class.
* type(mytype)   :: A, B
    * AandBare instances ofmytype
* A variable that is a member of the type/class is often called an _attribute_ .
* A _method_ is a subprogram that is a member of a class. Loosely refers to subprograms associated with a non-class type in a module.

# Types with Procedures

Types containing _type-bound procedures_ were introduced in Fortran 2003.  They are nearly synonymous with _methods_ in other languages.

Type-bound procedures can be renamed to another name to be used with an instance.

If a type-bound procedure is public it can be called in the conventional way from a unit that creates a variable of the type.

If the method is private then it can be accessed only via an instance of the type.

# Instance Parameters

In Fortran the instance variable must be passed explicitly as the first parameter to the method.

The instance variable must be declared asclassrather thantype.

When we invoke the method we do _no_ t explicitly pass the instance argument.

If it does not need to be passed at all (for the equivalent of a _class method_ in other languages), thenopassattribute can be added.

# Example

* modulemytype_class
* implicit none
* typeMyType
  * integer   ::i,j
  * real      ::x,y
  * contains
    * procedure ::init=>init_class
    * procedure :: write=>write_class
* end typeMyType
* privateinit_class
* contains
* subroutineinit_class(self,stuff1,stuff2)
  * class(MyType), intent(inout):: self
  * real, intent(in)            :: stuff1, stuff2
  * self%i=0;self%j=0
  * self%x=stuff1;self%y=stuff2
* end subroutineinit_class

# Example (P. 2)

subroutinewrite_class(self,iunit)

class(MyType), intent(in) :: self

integer,      intent(in) ::iunit

write(*,*) "Integers ",self%i,self%j

write(*,*) "Reals",self%x,self%y

end subroutinewrite_class

end modulemytype_class

…in caller:write_classis not private so the second two calls are equivalent.

callmyvar%init(x,y)

callwrite_class(myvar,11)

callmyvar%write(12)

# Data Hiding

One of the main purposes of OOP is to prevent outside units from doing anything without “sending a message” to an appropriate instance.

The previous example violates this principle.  We can make everything private, which means that only members of the module can access the symbols.  We must then go through an instance of the type/class to invoke the methods.

Making a type public “exposes” the type name and its type-bound procedures, but not its attributes.

We will modify the example to accomplish this.

# Modified Example

* modulemytype_class
* implicit none
* private  !Everything contained is now private
* public ::MyType!so need to make the type public
* typeMyType
* private  !Methods must be declared private
  * integer   ::i,j
  * real      ::x,y
  * contains
    * procedure ::init=>init_class
    * procedure :: write=>write_class
* end typeMyType
* contains
* subroutineinit_class(self,stuff1,stuff2)
  * class(MyType), intent(inout) :: self
  * real,          intent(in)    :: stuff1, stuff2
  * self%i=0;self%j=0
  * self%x=stuff1;self%y=stuff2
* end subroutineinit_class

# Modified Example, P. 2

subroutinewrite_class(self,iunit)

class(MyType), intent(in) :: self

integer, intent(in)       ::iunit

write(*,*) "Integers ",self%i,self%j

write(*,*) "Reals",self%x,self%y

end subroutinewrite_class

end modulemytype_class

…in caller:

callmyvar%init(x,y)

callwrite_class(myvar,11) ! illegal, link error

callmyvar%write(12)       ! OK

# Constructors and Destructors

* A constructor is a subprogram that handles the bookkeeping to initialize an instance of a type. This may entail:
  * Assigning values to attributes
  * Allocating memory forallocatablearrays
    * This _never_ happens automatically.  If anallocatableis a member of a type, a constructor must be written.
* A destructor is a subprogram that releases memory for a type.  This may be required if you allocate in a constructor.

# Constructors

Fortran has no special syntax for a constructor or destructor.  Programmers can define aninitfunction or equivalent, then declare itprivateto be sure it can be accessed only through a type instance.  Destructors can be similarly written to deallocate arrays.

# Exercise

* Write a class Atom that contains the following attributes:
  * Element symbol
  * Element name
  * Atomic mass
  * Atomic number
* The method should be
  * Compute and return the number of neutrons from the mass and number (n=mass-number)
* Also write a routine to initialize an instance of the class (a constructor).

# Inheritance

Inheritance is not restricted to classes in Fortran.

typeParenttype

integer ::my_id

real    ::my_value

end typeParenttype

typeChildtypeextends (Parenttype)

integer ::my_int

end typeChildtype

# Attribute Inheritance

The child type inherits all the attributes of its parent.

type(ChildType) ::billy

billy%my_idis valid, and is equivalent to

billy%ParentType%my_id

Butbilly%my_intdoes not refer back to the parent, since that variable occurs only in the extension.
