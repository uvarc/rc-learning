---
title: "Classes"
toc: true
type: book
weight: 73

menu:
    fortran_introduction:
        parent: Classes
        weight: 73
---

A _class_ is a data structure that encapsulates both data (variables) and behavior (procedures).  The only difference between an abstract type and a class is that a class may contain procedures as well as variables.

The major difference between a class and a module is that modules cannot be _instantiated_; that is, variables can be declared of any type it may encompass, but not of the module itself.

## OOP Terminology

An _instance_ of a type or class is a variable of that type/class.
```fortran
type(mytype)   :: A, B
```
A and B are instances of mytype.
A variable that is a member of the type/class is often called an _attribute_.
A _method_ is a procedure that is a member of a class. Loosely speaking, it can also refer to subprograms associated with a non-class type in a module.

## Types with Procedures

Types containing _type-bound procedures_ were introduced in Fortran 2003.  They are nearly synonymous with _methods_ in other languages.
If a type-bound procedure is public it can be called in the conventional way from a unit that creates a variable of the type.
If the method is private then it can be accessed only via an instance of the type.

Type-bound procedures are declared using CONTAINS and the PROCEDURE keyword.
```fortran
TYPE mytype
   REAL      ::x,y
   CONTAINS
      pROCEDURE ::init=>init_class
      PROCEDURE :: write=>write_class
      PROCEDURE :: reset
END TYPE
```
The `=>` operator is optional.  It means that the procedure on the left-hand side will be used through an instance to invoke the actual procedure on the right-hand side.
```fortran
TYPE(mytype) :: v
   v%init(arg1,arg2)
   v%write()
   v%reset(arg)
```

### Instance Parameters

In Fortran the instance variable must be passed explicitly as the first parameter to the method.
The instance variable must be declared as CLASS rather than TYPE.
When we invoke the method we do _not_ explicitly pass the instance argument.
If it does not need to be passed at all (for the equivalent of a _class method_ in other languages), the NOPASS attribute can be added.

Filling out the procedures gives us
```fortran
MODULE mytype_class
IMPLICIT NONE
   TYPE MyType
      INTEGER   ::i,j
      REAL      ::x,y
      CONTAINS
         PROCEDURE :: init=>init_class
         PROCEDURE :: write=>write_class
         PRECEDURE :: reset
   END TYPE

   PRIVATE init_class

   CONTAINS

   SUBROUTINE init_class(self,stuff1,stuff2)
      CLASS(MyType), INTENT(INOUT) :: self
      REAL,          INTENT(IN)    :: stuff1, stuff2
      self%i=0; self%j=0
      self%x=stuff1; self%y=stuff2
   END SUBROUTINE

   SUBROUTINE WRITE_CLASS(self,iunit)
      CLASS(MyType), INTENT(IN) :: self
      INTEGER,       INTENT(IN) ::iunit

      WRITE(*,*) "Integers ", self%i, self%j
      WRITE(*,*) "Reals", self%x, self%y
   END SUBROUTINE

   SUBROUTINE reset(self,i1,i2,stuff1,stuff2)
      CLASS(MyType), INTENT(INOUT) :: self
      INTEGER,       INTENT(IN)    :: stuff1, stuff2
      REAL,          INTENT(IN)    :: stuff1, stuff2
      self%i=i1; self%j=i2
      self%x=stuff1; self%y=stuff2
   END SUBROUTINE
END MODULE
```
There is no reserved word in Fortran for the class instance variable.  Some authors use "self" as is conventional in Python.  Others use "this" to imitate C++ and Java.  Others prefer their own conventions.

### Invoking Class Methods

The write_class method is not private so the second two calls are equivalent.
```
call myvar%init(x,y)
call write_class(myvar,11)
callmyvar%write(12)
```
Even in languages such as C++, where the "real" form `write_class(this,param)` is not accessible, the compiler still constructs a "real" function with a unique name for the method and invokes it in the usual way.  The class name is typically attached as well as the instance variable appearing explicitly.  The "true" name of the method is said to be "mangled."  Name mangling is also used to distinguish _overloaded_ functions which can seemingly take different types for the same calling sequence.  Various language rules, such as not passing the instance variable, are often called "syntactic sugar" by computer scientists, since their aim is to simplify the human-written code.
