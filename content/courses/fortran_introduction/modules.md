---
title: "Modules"
toc: true
type: docs
weight: 80

menu:
    fortran_introduction:
        name: Modules
        weight: 80

---

# Why Use Modules

Modules allow you to organize your code into logically-connected units.  It is a form of _object oriented programming_ .

Modules should contain coherent _data+procedures_  _._

Modules permit _data hiding_ .  Variables and subprograms may be kept private from other program units.  This prevents another source of error, by reducing the number of variables an outside program can affect or procedures it can call.

# Fortran Modules

Each module has a name that must be unique.  A module begins with

modulemodname

and ends with

end module

Modules are typically placed into separate files.  The file name does not need to be the same as the module name, but the module will be referenced by its name and not by the file name.

# Using Modules

Modules are brought in via theusestatement

usemymodule

All use statements must be the firstnonexecutablestatements after the declaration of the program unit (program, function, subroutine), beforeimplicit noneand the variable declarations.

There is no distinct namespace for a Fortran module.

# Variations of USE

Only specific routines can be brought in:

USEmymod, ONLY : f1, f2, s4

Routines can be renamed:

USEmymod, name-here => name-in-module

USEstats_lib,sprod=>prod

# Module Variables.

IMPLICIT NONEat the top applies throughout the module.

All variables declared or types defined before acontainsstatement are global throughout the module.

Module symbols (variables and names of routines) can be __private__ .  You may also explicitly declare them __public__ but that is the default.

Theprivateandpublicattributes may be added to the declaration, or they may be specified separately with a list following.

Private variables are not accessible by program units that use the module.  Only units in the same module can access them.

Example:

real, private  :: x, y, z

private        ::r_fun,d_fun

# Subprograms in Modules

Subprograms defined in a module must be within aCONTAINSclause.

Variables declared above theCONTAINSare global to all the subprograms.

Thefunctionorsubroutinekeywords afterendare _not_ optional, e.g.end subroutineis required.  The name of the procedure is still optional and some authors do not use it withend, in case it is changed later.

All subprograms in a module have an implicit interface.  You should not write an explicit interface for them (and in fact it’s illegal to do so).

# Example

modulemymod

implicit none

integer   ::Nmax=100000

contains

subroutinemysub(a,x)

real, dimension(:), intent(in) :: a

real                intent(out):: x

real, dimension(Nmax)          :: b

do stuff

end subroutinemysub

end modulemymod

# Exercise

* Type in the modulemymodinto a file mymod.f90
  * Fortran allows the module and the file to have either the same or a different name, but the name of the module is the name that must appear in the use statement.
* Fill out the subroutinemysubto set b to 11., then set x to the sum of corresponding elements of a and b.  Hint: you can use x=a(:)+b(:size(a)) to avoid a loop.
* Write a main program main.f90 that usesmymod, initializes Aallocatable, allocates it to 1000, sets its values to i+3, then passes it tomysub.   Print the value of x that is returned.
* Copy the exampleMakefile.  Make the appropriate changes to the program name, the names of the sourcefiles,andthe names of the object files.  Make thedepencencyline at the end
* main.o:main.omymod.o
* Run amakeproject inGeany.

