---
title: "Derived Types"
toc: true
type: book
weight: 72

menu:
    fortran_introduction:
        parent: Derived Types
        weight: 72

---
## Programmer-defined Datatypes

So far we have used only the predefined types available in the Fortran standard.  However, an important principle of modern software engineering is _encapsulation_.  We would like for related data to be connected.  This also allows the programmer to control the _interface_, the way in which other parts of the program interact with the data.

For example, consider a program to update employee information.  We can define several variables relevant for an employee; for example we might use salary, name of manager, name of department, employee ID number, and so forth.  Each of these is potentially a different type.  Salary would be floating point, the names would be strings, and the ID number would generally be an integer.  We have more than one employee to handle, so we must use some form of list or array.  In most languages we cannot define a single array to accommodate all these _fields_.  
This leads to the need for a way to keep all the information about one employee coordinated.

If we were restricted to predefined types in Fortran, we would have to declare separate arrays for each field of interest.  When processing data, we would have to take pains to ensure that the index of one array was correct for another array.  Suppose we wanted to find all employees making more than a certain amount.  We would have to search the "salary" array for the elements that met the criterion, while storing the index into some other array, so that we would have an array whose contents were the indices for other arrays.  This is very prone to errors. 
```fortran
integer,            dimension(:), allocatable:: employee_ID
character(len=128), dimension(:), allocatable:: employee_name
character(len=128), dimension(:), allocatable:: employee_manager
character(len=128), dimension(:), allocatable:: employee_dept
real,               dimension(:), allocatable:: employee_ID
```
We need a different type of data structure.  Programmer-defined datatypes allow the programmer to define a new type containing the representations of a group of related data items.
For example, some languages define _dataframes_, which are essentially representations of spreadsheets with each column defined as something like an array. This would be an example of a defined datatype, since it must be described relative to basic types available in the language.  This is perhaps easier in languages that use _inferred typing_, where the interpreter or compiler makes its best guess as to the type of data, as opposed to statically typed languages like Fortran or C++.  But conceptually it is a good example of a programmer-defined datatype.

In Fortran abstract datatypes are called _derived types_.  The syntax is extremely  simple; in the example, `ptype stands for a primitive type.
```
TYPE mytype
   <ptype> var1
   <ptype> var2
   <ptype>, DIMENSION(:), ALLOCATABLE:: var3
   TYPE(anothertype) :: var4
END TYPE mytype
```
Variables of `mytype` are declared as
```
type(mytype) :: x, y
```
We access the fields using the `%` separator:
```
z=x%var1
w=y%var2
allocate(x%var3(N))
```
where the variables `z` and `w` must be declared to match the type, including attributes such as ALLOCATABLE, of the field of the type.  As shown above, a TYPE may be a member of another TYPE as long as its definition has already been seen by the compiler.  Variables that belong to the type are usually called _components_ in Fortran.

Note that a type is a scoping unit.

We can apply this to our employee example.  The longer name for the fields is not helpful since we declare everything to be pertinent to an "employee."
```fortran
TYPE employee
   INTEGER             :: ID
   CHARACTER(len=128)  :: name
   CHARACTER(len=128)  :: manager
   CHARACTER(len=128)  :: dept
   REAL                :: salary
END TYPE
```
We can now declare employees
```fortran
TYPE(employee) :: fred, joe, sally
real           :: raise

   raise=0.02
   fred%salary=(1+raise)*fred%salary
```

## Arrays and Types

Types may contain arrays and from F2003 onward, those arrays may be allocatable. At the time of this writing, very few compilers do not support this standard but if one is encountered, the POINTER attribute must be used.  We will not discuss POINTER further but it may be seen in code written before F2003 compilers were widely available.

In Fortran, the array data structure is a _container_.  This means that the elements of an array may be derived types.  
```fortran
TYPE(employee), dimension(:), allocatable :: employees
```
We allocate as usual
```fortran
num_employees=126
allocate(employees(num_employees))
```

## Arrays and Modules

We nearly  always  put derived  types  into modules; the module will  define procedures that operate on the type. The module must _not_ have the same name as the derived  type, which can be somewhat inconvenient.  Call your module bird_dat.
For example, if you need to allocate memory, say for an allocatable array, to create a variable of a given type, this will _not_ happen automatically. You must write a _constructor_ to allocate the memory.
```
Example:
This type is a set of observations for birds denoted by their common name.
```fortran
TYPE bird_data
   CHARACTER(LEN=50)                  :: species
   INTEGER, DIMENSION(:), ALLOCATABLE :: obs
END TYPE 
```
A constructor-like procedure would be
```fortran
MODULE bird_obs

TYPE bird_data
   CHARACTER(LEN=50)                  :: species
   INTEGER, DIMENSION(:), ALLOCATABLE :: obs
END TYPE 

   CONTAINS

   SUBROUTINE init_bird(bird,species,obs)
      TYPE(bird_data),       INTENT(INOUT) :: bird
      CHARACTER(LEN=50),     INTENT(IN)    :: species
      INTEGER, DIMENSION(:), INTENT(IN)    :: obs
      
         bird%species=species
         allocate(bird%obs(size(obs)))
         bird%obs=obs
   END SUBROUTINE
END MODULE
```
It is important to understand that the `species` that is a member of the type is _not_ the same as the `species` that is passed in to `init_bird`.  In Fortran we can easily distinguish them since we _must_ use the instance variable, `bird` in this case, as a prefix; not all languages require that.  In C++ we would need to use `this->species` (`this` is the "invisible" instance variable in that language) if an attibute has the same name as a dummy parameter.

**Exercises**

1. Consider how you might create a "dataframe" type in Fortran.  One way to do this is to remember that a type may contain other types, so you could first define a "column" type.  This would be similar to the approach by the Pandas package for Python, in which a dataframe is a composite of "Series" objects.  You may assume that you will be writing this for a program that will use the same data layout (number of columns, datatype in each column, etc.) but for different data values and different lengths.

2. Write a main program to use the bird_dat module.  Assume you will read the bird data from a CSV (comma-separated values) file with each row consisting of a string for the species and then 10 numbers for observations over 10 years.  Create a file 
```
"BlueJay", 24, 23, 27, 19, 22, 26, 28, 27, 24, 30
"Cardinal", 11, 15, 18, 18, 19, 17, 20, 21, 20, 19
```
Use this file to test your program.

**Project**

Download the file [vabirds.csv](/data/vabirds.csv).
1. Create the derived type bird_data in a module bird_dat as illustrated in the example.  This is still a type; we do not need a class for this exercise. 
2. Add the following procedures to the constructor already written.
   * Write a `stats` procedure that takes only an instance of the type and returns the mean and standard deviation of the observations for that instance.
   * Write a minmax procedure that takes an instance of the type and the array of years and returns the maximum observed, the minimum observed, and the years for maximum and minimum.  You may use the maxval, minval, maxloc, and minloc intrinsics.
   * Write a main program that uses your module and also uses the sorters module that you can download ([sorters.f90](/courses/fortran_introduction/solns/sorters.f90)). This implements bubblesort.  Bubblesort is simple and slow but is more than sufficient for this exercise.  Note that the subprogram is destructive, i.e. it overwrites the array to be sorted, so make a copy if you don’t want that.

Remember to write an explicit interface for each subprogram in this “main” file.  Do not use CONTAINS. Read the file name from the command line.   First of all you will need to count the number of lines in the file.  Write a function count_lines that does this and returns the number.  It is up to you whether you pass it the number of header/footer lines.
Count_lines can check for the existence of the file and return 0 if it is not found.
Still in the read_data routine, using the number of items in the file, corrected for the header and the two footers, allocate an array of bird_data types.  
Loop through this array calling your constructor for each species.  
The read_data routine should return the array of years and the array of bird_data types.  

Request a species name from the user.  Find the species in your array of types 
and print its mean, standard deviation, and results from minmax. Print some appropriate message if the species is not found.  Compute an array of the means for all species.  
Use the pbsort routine from `sorters` to sort this array.  This procedure also returns the permutation vector, which is an array of the indices of the original positions.  
For example, if after the sort the permutation vector is (17,3,55,11,23, and so forth) that means that the element that was previously 17 is now the first in the new array, and so on.  
Note that these sorters return in ascending order (smallest to largest).  From the sorted mean array and the permutation index, print the names of the 10 most common (by mean) species over the years of observations.  
Hint: you can use a trick to reverse a dimension of an array in Fortran: 
```fortran
R=A(ndim:1:-1)
```
Test the user input portion for 
<br>
TurkeyVulture
<br>
TuftedTitmouse 
<br>
ElegantTrogon

For this project you can require an exact match of the species name.  (Note that no spaces are allowed and words are separated by capitalization; we would have to do more sophisticated string handling if we were to allow spaces and variations in capitalization.)

In addition to the sorters.f90 module mentioned above, the sample solution uses the [file_utils](/courses/fortran_introduction/solns/file_utils.f90) module that collects some useful file-related subprograms, including the count_lines function.

{{< spoiler text="Sample solution" >}}
{{< code file="/courses/fortran_introduction/solns/bird_dat.f90" lang="fortran" >}}
{{< code file="/courses/fortran_introduction/solns/bird_obs.f90" lang="fortran" >}}
{{< /spoiler >}}
