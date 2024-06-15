---
date : "2021-06-23T00:00:00-05:00"
title: "User-Defined Types: Structs"
toc: true
type: book
weight: 84

---

Even the standard templated types such as vectors are not sufficiently flexible for many applications; while they can be set up for several different underlying types, all elements must consist of a single type.  Frequently we wish to collect different types together in some form of cohesive data structure.  To address this, a variety of avenues are available for programmers to define their own types.

For example, consider a program to update employee information.  We can define several variables relevant for an employee; for example we might use salary, name of manager, name of department, employee ID number, and so forth.  Each of these is potentially a different type.  Salary would be floating point, the names would be strings, and the ID number would generally be an integer.  We have more than one employee to handle, so we must use some form of list or array.  In most languages we cannot define a single array to accommodate all these _fields_.  
This leads to the need for a way to keep all the information about one employee coordinated.

If we were restricted to generally-available types in C++, we would have to declare separate vectors for each field of interest.  When processing data, we would have to take pains to ensure that the index of one vector was correct for another vector.  Suppose we wanted to find all employees making more than a certain amount.  We would have to search the "salary" vector for the elements that met the criterion, while storing the indices into some other vector, so that we would have a vector whose contents were the indices for another vector.  This is very prone to errors. 
```c++
std::vector<int>         employee_ID;
std::vector<std::string> employee_name;
std::vector<std::string> employee_manager;
std::vector<std::string> employee_dept;
std::vector<float>       employee_salary;
```
We need a different type of data structure.  Programmer-defined datatypes allow the programmer to define a new type containing the representations of a group of related data items.
For example, some languages define _dataframes_, which are essentially representations of spreadsheets with each column defined as something like an array. This would be an example of a defined datatype, since it must be described relative to basic types available in the language.  This is perhaps easier in languages that use _inferred typing_, where the interpreter or compiler makes its best guess as to the type of data, as opposed to statically typed languages like Fortran or C++.  But conceptually it is a good example of a programmer-defined datatype.

In C++ abstract user-defined types are called _structs_.
The syntax is extremely simple (`ptype` stands for a base type):
```no-highlight
struct MyType{
  <ptype> var1;
  <ptype> var2;
};
```
Be sure to remember the semicolon after the closing curly brace.

We can apply this to our employee example.  The longer name for the fields is not necessary or helpful since we declare everything to be pertinent to an "employee."
```c++
struct Employee {
   int     ID;
   string  name, manager, department;
   float   salary;
};
```

Each variable belonging to a struct is called a _member_.
It is customary for the name of a struct (or class) to be capitalized, or to use "camel case."

Note that a struct is a scoping unit.

## Declaring Types and Accessing Fields

We declare variables much as for base types.  Such variables are often called _instances_ of that struct.

```c++
Employee  fred, joe, sally;
```
In C the `struct` keyword must be repeated before the struct name, but that is not required in C++ as long as there is no ambiguity.

Variables of a struct may also be declared when the struct is defined.
```c++
struct Employee {
   int     ID;
   string  name, manager, department;
   float   salary;
} moe, curly, larry;
```
To access the members of the struct use the name of the struct, a decimal point as a separator, and the name of the member.

```c++
   fred.name="Frederick Jones";
   fred.ID=1234;
   fred.department="Accounting";
   fred.salary=75200.00;
```

## Initializing Variables

In C, struct members could not be initialized at declaration.  In C++11 and above, they can be.

```c++
struct GridPoint {
   int x=0., y=0., z=0.;
}
```
They may also be initialized when an instance is declared using curly braces, in order of declaration in the struct.
```c++
GridPoint p={1.,1.,1.}
```

## Structs in Structs

Struct members may be instances of other structs.  The definition of the member struct must have been visible to the compiler before it is included in another struct.
```c++
struct Address {
string streetAddress;
string city, state;
int zipCode;
};

struct Employee {
string name, department;
int ID;
float salary;
Address homeAddress;
};
```

## Pointers and the Arrow Operator

As for other types, variables can be declared pointer to struct:
```c++
Employee *jane;
```
This is particularly common when passing struct (and class) instances to functions, to avoid a possibly expensive copy.

When using a pointer, the `.` operator is replaced with the _arrow operator_
```c++
jane->name="Jane Smith"
```

## Arrays and Vectors of Structs

Arrays may be declared of struct types.

This example assumes a C++11 compiler for the initialization.  
{{< code file="/courses/cpp-introduction/codes/employees.cxx" lang="c++" >}}

Notice in the vector example, we create an element with `push_back` by invoking the _default constructor_.  In C++ a struct has many of the same properties as a _class_, but we will defer discussing this until we cover [classes](/courses/cpp-introduction/classes).

**Exercise**

Copy the [vabirds.csv](/data/vabirds.csv) file. Each line of this file consists of a string followed by a number of integers.  In the header (one line) the string is the word "Species" with the integers a sequence of years.  In subsequent lines, the string is the common name of the bird, and the integers are observations for the years indicated in the header.  Write a program that will
1. create a struct birdDat with members "species" and a vector for the observations.
2. create a vector of the struct.
3. Read the file name from the command line.
4. read the file.  Store the years from the header in a vector.  Load the data for each line into an element of the birdDat struct vector.    
5. Print the observations for AmericanCrow.

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/cpp-introduction/solns/read_bird_data.cxx" lang="c++" >}}
{{< /spoiler >}}

