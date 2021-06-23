---
title: "Modules"
toc: true
type: book
weight: 71

menu:
    fortran_introduction:
        parent: Modules
        weight: 71

---

Modules are subordinate program units that can contain multiple subprograms as well as associated variables.  
Modules allow you to organize your code into logically-connected units.  It is a form of _object oriented programming_.
They should contain coherent _data+procedures_.
Modules permit _data hiding_.  Variables and subprograms may be kept private from other program units.  This prevents another source of error, by reducing the number of variables an outside program can affect or procedures it can call.

## Fortran Modules

Each module has a name that must be unique.  A module begins with
```fortran
MODULE modname
```
and ends with
```fortran
END MODULE [NAME]
```

Modules are typically placed into separate files.  The file name does not need to be the same as the module name, but the module will be referenced by its name and not by the file name.  It is acceptable for short, closely-related modules to be in the same file.  If more than one module is in a file, each must be USEd individually.

## Using Modules

Modules are brought in via the USE statement
```fortran
USE mymodule
```
All USE statements must be the first nonexecutable statements after the declaration of the program unit (program, function, subroutine), before any IMPLICIT statement and the variable declarations.

There is no distinct "namespace" for a Fortran module.  Names imported into the USEing unit do not acquire a distinguishing name.

### Variations of USE

Only specified routines can be brought in with ONLY:
```fortran
USE mymod, ONLY : f1, f2, s4
```

Routines can be renamed:
```fortran
USE mymod, name-here => name-in-module
USE stats_lib,sprod=>prod
```

## Module Variables.

IMPLICIT NONE at the top applies throughout the module.
All variables declared or types defined before a CONTAINS statement are global throughout the module.

Module symbols (variables and names of procedures) can be __private__ .  You may also explicitly declare them __public__ but that is the default.
The private and public attributes may be added to the declaration, or they may be specified separately with a list following.
Private variables are not directly accessible by program units that use the module.  Only procedures in the same module can access them.
Using PRIVATE or PUBLIC as a separate statement without a list sets or resets the default and may be done only once per module.  Public and private may only be set in the specification (interface) portion of the module, not in the procedure bodies.

We will discuss PUBLIC and PRIVATE in more detail when we cover [classes](/courses/fortran_introduction/more_classes).

Example:
```
MODULE mymod
USE precisions
   REAL, PRIVATE  :: x, y, z
   REAL(sp)       :: r_fun
   REAL(dp)       :: d_fun
   PRIVATE        ::r_fun,d_fun
```
where we assume the `precisions` module defines the KIND parameters `sp` and `dp`.

## Subprograms in Modules

Subprograms defined in a module must follow a CONTAINS.
The FUNCTION or SUBROUTINE keywords after END are _not_ optional, e.g. END SUBROUTINE is required.  The name of the procedure is still optional and some authors recommend not using it, in case it is changed later or to avoid cut and paste errors.

All subprograms in a module have an __implicit interface__.  You should *not* write an explicit interface for them, and in fact it’s illegal to do so.

Example

{{< code file="/courses/fortran_introduction/codes/module.f90" lang="fortran" >}}

## Modules and Make

A module must be compiled _before_ any other file that uses it.  This can create a complicated build environment, so `make` or a similar build manager is usually used.

**Exercises**

1. Type the module `mymod` into a file `mymod.f90`.
Fortran allows the module and the file to have either the same or a different name, but the name of the module is the name that must appear in the use statement.
2. Fill out the subroutine `mysub` to set b to 11., then set x to the sum of corresponding elements of a and b.  Hint: you can use x=a(:)+b(:size(a)) to avoid a loop.
3. Write a main program `main.f90` that uses `mymod`, initializes `A` allocatable, allocates it to 1000, sets its values to `i+3` in a loop, then passes it to `mysub`.   Print the value of `x` that is returned.
4. Create a Makefile.  If you wish you may copy the example Makefile from the earlier [chapter](/courses/fortran_introdution/make).  Make the appropriate changes to the program name, the names of the source files, and the names of the object files.  Make the dependency line at the end
```make
main.o:main.o mymod.o
```
Run a make project in Geany or your preferred IDE.

**Project**

Download the file [bodyfat.csv](/data/bodyfat.csv).  This is a dataset of body fat, age, height, and weight for a set of participants in a study. BMI categories are as follows:

|Severely underweight |  BMI < 16.0 |
|Underweight          | 16 <= BMI < 18.5 |
|Normal               | 18.5 <= BMI < 25 |
|Overweight           | 25 <= BMI < 30 |
|Obese Class I        | 30 <= BMI < 35 |
|Obese Class II       | 35 <= BMI < 40 |
|Obese Class III      | BMI > 40       |

Write a `bmi_calculator` module containing functions/subroutines for the following:
1. Convert pounds to kilograms.  Use the actual conversion factor, not the approximate one.  Look it up on Google.
2. Convert feet/inches to meters.  Look up the conversion factor, do not guess at it.   
3. Compute BMI.
4. Determine where the user falls in the table supplied and return that informationin an appropriate form. 

Write a module `stats` that implements the following:
1. Mean of an array 
2. Standard deviation of an array 
3. Outlier rejection using Chauvenet’s criterion.  Pseudocode given further down.
Make as much use of Fortran intrinsics/array operations as you can.

Write a main program that implements the following:
1. Uses your modules
2. Reads the input file into appropriate allocatable arrays (use one-dimensional arrays for this project).  Don't assume you know the length of the file (but you can assume the number of header lines is fixed).  
3. Pass appropriate arrays to a subroutine that computes an array of BMI data based on height and weight and returns the BMI array.
4. Rejects the outlier(s).  The function should return an array of logicals that you can apply to the original data using WHERE or similar.  Create new arrays with the outlier(s) deleted. 

Write a file that contains the corrected data for bodyfat and BMI.  Use Excel or whatever you normally use to plot BMI as a function of percentage body fat. 
Be sure to plot it as a scatter plot (points only, no connecting lines).  

Chauvenet’s criterion: It’s not the state of the art but works pretty well.
1. Compute the mean and standard deviations of the observations.
2. Compute the absolute values of the deviations, i.e. abs(A-mean(A))/std(A)
3. Use the tails `devs=devs/sqrt(2.)`
4. Compute the probabilities `prob=erfc(devs)` : erfc is an intrinsic in any fairly recent Fortran compiler.  
5. The criterion is that we retain data with `prob>=1./(2*N_obs)` (number of observations).

{{< spoiler text="Example solution" >}}
{{< code file="/courses/fortran_introduction/solns/stats.f90" lang="fortran" >}}
{{< code file="/courses/fortran_introduction/solns/bmi_calculator.f90" lang="fortran" >}}
{{< code file="/courses/fortran_introduction/solns/bmi_data.f90" lang="fortran" >}}
{{< /spoiler >}}
