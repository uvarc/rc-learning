---
title: "User-Defined Types: Enums and Typedef"
toc: true
type: book
weight: 83

---

The predefined types available in C++ are not sufficient for most non-trivial programming requirements.  
In C++ terminology, nearly any type that is not a native type is said to be _user-defined_.  Arrays, pointers, and references fall into this category.
We have already discussed those types and will focus here on more advanced user-defined types.

## Enumerations


One of the simplest user-defined types is the enumeration or _enum_.
An enumeration associates integers with names.  By default, the integers begin at 0 and increment by 1 until each name has been assigned a value.

### Unscoped Enums

For compatibility with C, C++ supports _unscoped enums_.  These are in scope
throughout the unit in which they are declared.

```c++
enum WeekDays { Sun, Mon, Tue, Wed, Thu, Fri, Sat };
```
Quotes are not used for the enum names because these are not literal strings.

Variables can be declared of an enum type.
```
WeekDays today;
```
The possible values of an enum variable are restricted to the integers declared in the enum.
```c++
today=Fri
std::cout<<today<<"\n";
```
Enums can start at a value other than 0.
```c++
enum Spring { Mar=3, Apr, May };
```
If a value is specified and subsequent ones are not, the rule of incrementing by one is followed.
```c++
enum Cards { Diamonds, Spades=4, Clubs, Hearts };
```
Diamonds=0, Spades=4, Clubs=5, and Hearts=6.

Specific values may be given to each name.  The same value may be reused, though this is usually not a good idea.
```c++
enum MonthDays { Jan=31, Feb=28, Mar=31, Apr=30, May=31};
```

### Scoped Enums

Unscoped enums can result in conflicts.  Consider the example of
```c++
enum color {red, yellow, orange};
enum fruit {apple, orange, pear};
```
The compiler does not allow this, because "orange" has already been declared when it sees the second enum.  

To handle this, C++11 introduced the _scoped enum_ or _class enum_.  This uses the keyword `class`.
```c++
enum class color {red, yellow, orange};
enum class fruit {apple, orange, pear};
```
We then use the scope-resolution operator to refer to the names in the enum.
```c++
color paint == color::red;
```

An enum is a _type_ and therefore converting its values to another type generally requires a cast.  Unscoped enums can be cast to int implicitly, but a scoped enum must use an explicit cast.  
{{< code-download file="/courses/cpp-introduction/codes/enum.cxx" lang="c++" >}}

The `static_cast` is a cast that occurs at compile time.  For native types it is essentially the same thing as the ordinary cast.  It can also be used for user-defined types, as we have illustrated for the enum in the above example.  It takes a templated type in angle brackets as the indicator to which it should cast its argument. 

## Typedef

It may be convenient to name a new user-defined type, or to rename an existing type, so that new variables can be declared by name.  We use `typedef` to create a new name for our type.  The syntax is
```no-highlight
typedef existing newname
```
We can then declare variables of `newname` type.

Examples:

```c++
typedef float real;
typedef boost::multi_array<double,2> Array2D;

   real x;
   Array2D gridArray(boost::extents[nrows][ncols]);
```

Typdef is particularly useful when working with templated types whose declarations may be long and awkward, as in the Boost array example above.  Unlike user-defined types such as [structs](/courses/cpp-introduction/structs) the new "type" is merely an synonym for an existing type.

Typedefs are very common in C code, because C requires using the `struct` keyword to declare variables of that type.
```c
struct myStruct {
   int myvar;
   float anothervar;
};

struct myStruct var1;
```
This would frequently be declared with
```c
typedef struct myStruct {
   int myvar;
   float anothervar;
} aStruct;

aStruct var1;
```

In contrast to C, C++ does not require the `enum`, `struct`, and `class` keywords in variable declarations, as long as there is no ambiguity.  Programmers are advised to avoid creating this ambiguity, but some libraries may not adhere to this principle.  Ambiguity can commonly occur when a struct contains a member with the same name as a typedef.

```c++
struct myStruct {
   int myvar;
   float anothervar;
}

myStruct var1;
```

Structures are described in detail in the next chapter.
