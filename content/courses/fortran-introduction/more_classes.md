---
title: "Data Hiding and Inheritance"
date : "2021-04-5T00:00:00-05:00"
toc: true
type: book
weight: 76

menu:
    fortran-introduction:
        parent: Data Hiding and Inheritance
        weight: 76
---

Classes are fairly new to Fortran and we will mention only a few more of their features.  Some, such as inheritance, are applicable to all types.  We will describe some additional features below. 

## Data Hiding

One of the main purposes of OOP is to prevent outside units from doing anything without “sending a message” to an appropriate instance.
As for modules, we can specify access through the PUBLIC and PRIVATE statements or attributes.  As for modules, the default is PUBLIC.
The previous example violates this principle.  We can make everything private, which means that only members of the module can access the members of the class.  We must then go through an instance of the type/class to invoke the procedures.
Making a type public “exposes” the type name and its type-bound procedures, but not its variables if the module default is set to private.
We will modify the example to accomplish this.

### Modified Example

{{< code-download file="/courses/fortran-introduction/codes/private_class_module.f90" lang="fortran" >}}

In the caller:
```fortran
CALL myvar%init(i,j,x,y)
CALL write_class(myvar,11) ! illegal, link error
CALL myvar%write(12)       ! OK
```

## Constructors and Destructors

A _constructor_ is a subprogram that handles the bookkeeping to initialize an instance of a type. This may entail:
  * Assigning values to variables
  * Allocating memory for allocatable arrays
    * This _never_ happens automatically.  If an allocatable is a member of a type, a constructor must be written.

A _destructor_ is a subprogram that releases memory for a type.  This may be required if you allocate in a constructor.  The garbage collection in subprograms will not release memory allocated for a type.

Fortran has no special syntax for a constructor or destructor.  Programmers can define an `init` function or equivalent, then declare it `private` to be sure it can be accessed only through a type instance.  Destructors can be similarly written to deallocate arrays.

## Inheritance

_Inheritance_ is when a type is derived from another type and has access to its members.  Inheritance is not restricted to classes in Fortran but can be used with types as well.
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

When a class is extended, not only the components but the procedures are inherited.
{{< code-download file="/courses/fortran-introduction/codes/clinherit.f90" lang="fortran" >}}

**Exercise**

* Write a class Atom that contains the following attributes:
  * Element symbol
  * Element name
  * Isotopic mass (mass of a single isotope, not the "atomic weight" averaged over a mix of isotopes)
  * Atomic number
* The method should be
  * Compute and return the number of neutrons from the mass and number (n=mass-number)
