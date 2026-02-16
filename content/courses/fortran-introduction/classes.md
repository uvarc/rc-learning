---
title: "Classes"
date : "2021-04-05T00:00:00"
toc: true
type: book
weight: 75

menu:
    fortran-introduction:
        parent: Classes
        weight: 75
---

A _class_ is a data structure that encapsulates both data (variables) and behavior (procedures).  The only difference between a derived type and a class is that a class may contain procedures as well as variables.

The major difference between a class and a module is that modules cannot be _instantiated_; that is, variables can be declared of any type it may encompass, but not of the module itself.

## OOP Terminology

An _instance_ of a type or class is a variable of that type/class.
```fortran
type(mytype)   :: A, B
```
A and B are instances of mytype.
The variables and procedures that belong to the class are often called _members_ of the class.

## Types with Procedures

Types containing _type-bound procedures_ were introduced in Fortran 2003.  They are nearly synonymous with _methods_ in other languages.
If a type-bound procedure is public it can be called in the conventional way from a unit that creates a variable of the type.
If the procedure is private then it can be accessed only via an instance of the type.
As for modules, the default is `public`.  

Type-bound procedures are declared using CONTAINS and the PROCEDURE keyword.
```fortran
TYPE mytype
   REAL      ::x,y
   CONTAINS
      PROCEDURE ::init=>init_class
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

In Fortran the instance variable must be passed explicitly as the first parameter to the procedure, unless the PASS keyword is utilized.
The instance variable must be declared as CLASS rather than TYPE.
When we invoke the procedure we do _not_ explicitly pass the instance argument.
If it does not need to be passed at all (for the equivalent of a _class method_ in other languages), the NOPASS attribute can be added, as discussed below.

Filling out the procedures gives us
{{< code-download file="/courses/fortran-introduction/codes/class_module.f90" lang="fortran" >}}

There is no reserved word in Fortran for the class instance variable.  Some authors use "self" as is conventional in Python.  Others use "this" to imitate C++ and Java.  Others prefer their own conventions.

### Invoking Class Methods

The write_class procedure is not private so the second two calls are equivalent.
```
call myvar%init(x,y)
call write_class(myvar,11)
callmyvar%write(12)
```
Even in languages such as C++, where the "real" form `write_class(this,param)` is not accessible, the compiler still constructs a "real" function with a unique name for the method and invokes it in the usual way.  The class name is typically attached as well as the instance variable appearing explicitly.  The "true" name of the method is said to be "mangled."  Name mangling is also used to distinguish _overloaded_ functions which can seemingly take different types for the same calling sequence.  Various language rules, such as not passing the instance variable, are often called "syntactic sugar" by computer scientists, since their aim is to simplify the human-written code.

### PASS and NOPASS

If it is desirable to pass the instance variable at some position other than the first, the PASS attribute may be used.

```fortran
module mytype_mod
implicit none
TYPE mytype
   contains
      procedure, pass(z) :: mysub
end TYPE

   contains

      subroutine mysub(y,z)
      class(mytype) :: z
      real          :: y
         print *, y,z
      end subroutine
end module
```
In this case `z` will represent the instance variable, so the procedure will be invoked like
```fortran
call z%mysub(y)
```

For cases where no instance variable is required at all, the NOPASS attribute may be used.  This creates something analogous to a type of method that is sometimes called a _class method_ in other languages.
```fortran
module mytype_mod
implicit none
TYPE mytype
   contains
      procedure, nopass :: mysub
end TYPE

   contains

      subroutine mysub(y,z)
      real          :: y,z
         print *, y,z
      end subroutine
end module
```
Invoke this with
```fortran
call t%mysub(w,x)
```
If `mysub` is public it can even be called with
```fortran
call mysub(w,x)
```
in which case it is effectively equivalent to a procedure in a module  
