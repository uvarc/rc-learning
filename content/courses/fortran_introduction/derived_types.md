---
title: "Derived Types"
toc: true
type: book
weight: 73

menu:
    fortran_introduction:
        parent: Derived Types
        weight: 73

---
# Abstract Types

In Fortran abstract types are called _derived types_.  The syntax is extremely  simple; in the example, `ptype stands for a primitive  type.
```
TYPE mytype
<ptype> var1
<ptype> var2
<ptype>, DIMENSION(:), ALLOCATABLE:: var3
TYPE(anothertype) :: var4
END TYPE mytype
```

We nearly  always  put derived  types  into modules; the module will  define functions that operate on the type. The module must _not_ have the same name as the derived  type, which can be somewhat inconvenient. If you need to allocate memory, say for an allocatable array, to create a variable of a given type this will not happen automatically. You must write a _constructor_ to allocate the memory.
```
type(mytype)                 :: thevar
type(mytype), dimension(100) :: var22
type(mytype), dimension(:), allocatable::var11

To access the fields of the type use the name of the type, the percent sign as a separator,  and the name of the field.
thevar%var2
var11(12)%var1
var22(1)%var4%varx 

This type a set of observations for birds denoted by their common name.
type bird_datacharacter(len=50):: species
integer, dimension(:), allocatable:: obs
end type bird_data
