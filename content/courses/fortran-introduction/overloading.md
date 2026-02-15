---
title: "Overloading and Generic Programming"
date : "2021-04-05T00:00:00-05:00"
toc: true
type: book
weight: 77

menu:
    fortran-introduction:
        parent: Overloading and Generic Programming
        weight: 77
---

Overloading is when a procedure or operator is able to work on multiple types.  Most languages overload at least basic arithmetic operators to work on all numerical types.  In modern Fortran the mathematical intrinsics are overloaded
to work at least on real and double precision, and when appropriate complex. For example, in older Fortran there were three versions of every trigonometric function when defined for complex numbers.  Cosine was COS for real number, DCOS for doubles, and CCOS for complex.  In modern Fortran COS works for all three.
This is an example of _generic_ programming.

Overloading can be more general than just numerical types.  In some languages, _sort_ is an intrinsic and works on any types for which relational operators (<,<=,>,>=) are defined.

Programmers can overload their own procedures in Fortran.  To make a generic procedure, an interface is required.  In this case the interface block has a name, which will be the generic name, and only the _specific_ procedures should be included.

In all the examples below, our new generic `diag` for computing the diagonal of a matrix, will accept either real or double precision arrays.  The extension to complex should be obvious if it is needed.

### Explicit Interface Block

We can use an explicit interface block in a non-module program unit.
```fortran
interface diag
    function rdiag(A)
       use precisions
       implicit none
       real (sp), dimension(:,:), intent(in) :: A
       real (sp), dimension(size(A,2))       :: rdiag
    end function
    function ddiag(A)
       use precisions
       real (dp), dimension(:,:), intent(in) :: A
       real (dp), dimension(size(A,2))       :: ddiag
    end function
end interface
```

### Module Procedure

Overloaded procedures are usually and most appropriately defined in a module.  Since procedures in modules already have an implicit interface, we cannot use the explicit interface above.  We still use a named interface block with MODULE PROCEDURE
```fortran
interface diag
    module procedure rdiag
    module procedure ddiag
end interface
```
The body of the functions would be in the module.

### Generic in Type-Bound Procedures

If we wanted to define something with various linear-algebra functions defined on it, we would use the GENERIC keyword.  Note that the specific functions must be written so that their **signatures**, the number and types of their arguments, are differ, or the compiler will not be able to distinguish them.
```fortran
type something
   !variables
   contains
      private
      procedure :: rdiag
      procedure :: ddiag
      generic,public :: diag=>rdiag,ddiag
end type
```

## Operator Overloading

The arithmetic operators `+`,`-`,`*`,and `/` can be overloaded to work on programmer-defined derived types.  Assignment (=) can also be overloaded when copying can be defined for the type.

### Module Procedures

The arithmetic operators are overloaded with a generic interface, but rather than the name of the generic function, the keyword OPERATOR is used, followed by the symbol in parentheses for operators, or ASSIGNMENT(=) for copying.
```fortran
interface operator(+)
   module procedure adder
end interface
interface operator(-)
   module procedure subber
end interface
interface assignment(=)
   module procedure assigner
end interface
```
The procedures that define the operator must be _functions_, must have two arguments, and must declare those arguments INTENT(IN).  For example, suppose we wished to define `adder` for an Atom type to add the atomic masses:
```fortran
type(Atom) function adder(atom_A,atom_B)
    type(Atom), intent(in) :: atom_A, atom_B
    adder%atomic_mass=atom_A%atomic_mass+atom_B%atomic_mass
end function
```

For assignment, the procedure must be a _subroutine_ with two arguments.  The first argument must represent the left-hand side and be INTENT(OUT), while the second represents the right-hand side and is INTENT(IN).
```fortran
subroutine assigner(new_atom,old_atom)
   type(Atom), intent(out) :: new_atom
   type(Atom), intent(in)  :: old_atom
   new_atom%symbol=old_atom%symbol
   new_atom%eng_name=old_atom%eng_name
   new_atom%atomic_number=old_atom%atomic_number
   new_atom%atomic_mass=old_atom%atomic_mass
   new_atom%electronegativity=old_atom%electronegativity
end subroutine
```

### Type-Bound Operators

Operators may be overloaded to work on class members.  The syntax is somewhat different from that for separate types in modules.
Here is a snippet from a module defining a Fraction class.  The rules for the arguments are the same as for modules, but of course the instance variable must be declared CLASS.
```fortran
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
```
