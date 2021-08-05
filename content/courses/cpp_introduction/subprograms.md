---
title: "Subprograms"
toc: true
type: book
weight: 61

menu:
    cpp_introduction:
        parent: Subprograms
        weight: 61

---

A _subprogram_ is a self-contained, but not standalone, program unit.  It performs a specific task, usually by accepting _parameters_ and returning a result to the unit that invokes (calls) it.
Subprograms are essential to good coding practice.  Among other benefits, they are
  * Reusable.  They can be called anywhere the task is to be performed.
  * Easier to test and debug than a large, catch-all unit.
  * Effective at reducing errors such as cut and paste mistakes.

Other general names for subprograms are _routines_, _procedures_, and _methods_. The word "method" is generally reserved for procedures defined within an _object_, but it is not conceptionally different from any other subprogram. 

Subprograms must be invoked or _called_ in order for any of their code to be executed.  

## Functions and Subroutines

Functions take any number (up to compiler limits) of arguments and return one item.  This item can be a compound type.
Functions must be declared to a type like variables.  The `return` statement returns the result to the caller.

Subroutines take any number of arguments (up to the compiler limit) and return any number of arguments.  Bidirectional communication can take place through the argument list.
Strictly speaking, all subprograms in C++ are functions, but the ability to declare a `void` return "type" and to pass by reference means some are effectively subroutines. 

Variables in the argument list are often called _dummy arguments_ since they stand for _actual arguments_ that are defined in the calling unit.
Like any variable, they must be declared explicitly.  In C++ these declarations are included in the argument list.
In C++ either the function or its _prototype_ must appear before any invocation.
The prototype consists only of the declaration of the function along with its argument list.  Only the types are required in the argument list of a prototype; dummy variables are optional.  The prototype provides the compiler with information about the number and type of arguments to the function, which enables it to check each invocation to ensure that the argument lists match.

C++ does not define or expect a keyword for a function declaration. The parentheses following the name are required, however.

```c++
//Prototype
float myfunc(float, float, float);

//Definition
float myfunc(float v1, float v2, float v3) {
    return v1*v2-v3;
}
```
The prototype is frequently declared in a header file ending in `.h` or `.hpp`. The function definition is generally then in a corresponding `.cpp` or `.cxx` file. 

Functions are invoked by name.  They can enter into an expression anywhere on the right-hand side of the assignment operator (=).
```c++
z=4.*myfunc(var1,var2,var3)
```
The names of the actual arguments when the function is invoked need not be the same as those of the dummies, but the number and type must match.

A "subroutine" would be declared `void` (so it has no return value).  If it is a utility routine, such as to print a message, it does not require an argument list.  In C++ (but _not_ C) an empty argument list in the declaration is equivalent to a single `void` argument.

{{< code-download file="/courses/cpp_introduction/codes/printme.cxx" lang="c++" >}}

## Default (Optional) Arguments

The programmer can provide default values for dummy arguments in a function.  If not passed, these take their default values.

```c++
int myfunc(int i, int j=0, int k=1) {
//code
}
```
This function could be called as
```c++
myfunc(i,j,k);
myfunc(i,j);
myfunc(i);
```
Since they are not required, default arguments are also said to be _optional_.  Values passed explicitly through the argument list override the defaults.

Default arguments may be set in either the declaration (prototype) or the definition of the function, but not both.  The values are set at compile time.
{{< code-download file="/courses/cpp_introduction/codes/default_args.cxx" lang="c++" >}}

C++ does not support what other languages call "keyword" arguments. They must be kept in position and may not be rearranged or skipped.

**Exercise**

Write a program that evaluates the function
$$f(x)=\frac{1}{\pi (1+x^2)}$$
for 401 values of x equally spaced between -4.0 and 4.0 inclusive.  
Put the values into an array or vector variable `x`.  Use variables for the starting and ending values of x and the number of values.  
Write a function to evaluate f(x) for any given real (scalar) value of x and call it each time through your loop.

Print the values and the corresponding function evaluation to a comma-separated-values (CSV) file.  Use software such as Excel, Python, Matlab, or anything else you know to plot the result.
