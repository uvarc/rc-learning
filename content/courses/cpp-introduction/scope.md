---
date: "2021-06-23"
title: "Variable Scope and Namespaces"
weight: 76
---

## Variable Scope

**Scope** is the range over which a particular variable is defined and can have a value.  In C++, scope is defined by _code blocks_, which are marked by curly braces `{}`.  Function parameter lists are also scoping units, as are certain statements (for, while, if, switch).

In the case of functions, the calling unit may have a variable named `x`, and a function may also have a variable named `x`, and if `x` is not an argument to the function then it will be distinct from the `x` in the calling unit.
```c++
x=20.;
float z=myfunc(x);

etc.

float myfunc(float y) {

float x=10.;  //can declare locals anywhere
return x*y;
}
```
A variable that is only in scope in a particular block is said to be _local_ to that unit.  Variables that are visible from more than one block are _global_ to those blocks.

Global variables are "in scope" within the file where they are declared.  To make them visible in other files, they should be placed within a header and declared with the `extern` keyword.
```c++
extern float myglobal;
```
As a general rule, globals over multiple files should be avoided.  Defining them in a [class](/courses/cpp-introduction/classes) would be preferable.  C++20 and up will add _modules_, which would be the most appropriate structure for this type of use.  Modules are familiar to Python and Fortran programmers, to name two languages that support them; they are a programming construct that isolates a unit of code, separating it into a separately compiled bundle, from which other program units can import its variables and functions.

Within a scoping unit, if a local variable is defined with the same name as a global variable, the local variable takes precedence.  The global variable can be accessed with the _scope resolution_ operator `::`.

{{< code-download file="/courses/cpp-introduction/codes/scope.cxx" lang="c++" >}}

Beware of changes in behavior in `for` loops.  Prior to the C++98 standard, declaring a loop variable within the parentheses left it in scope.  So code such as the following was legal:
```c++
for (int i=0; i<10; i++)
{
   // code
}

x = (float)i;
```
Old code tends to persist so programmers may encounter this. The solution is to declare `i` outside.
```c++
int i;
for (i=0; i<10; i++)
{
   // code
}

x = (float)i;
```

## Namespaces

We have seen the scope-resolution operator before:
```c++
std::cout<<"Print something\n";
```
In this case, `std` is a **namespace**.  A namespace is something like the "family name" of a group of variables and/or functions.  The _standard_ namespace `std` includes all standard C++ functions and data structures and is brought in through standard headers.

A _fully qualified_ name includes all namespace references.
```c++
std::vector<std::string> words;
```

By employing namespaces, different libraries can define functions with the same name.  The "family name" namespace can then differentiate them.

### Defining a Namespace

You can define your own namespaces.  Most usually the elements are classes, which we have not yet discussed, but that is not necessary.

```c++
namespace blue {
   float x,y;
}
namespace yellow {
   float x,y;
}
```
Notice that we can have variables with the same names in different namespaces, because a namespace is a scoping unit.  We must reference the variables with their namespace name:
{{< code-download file="/courses/cpp-introduction/codes/namespace.cxx" lang="c++" >}}

Namespaces can be nested:
{{< code-download file="/courses/cpp-introduction/codes/nestednamespace.cxx" lang="c++" >}}

### The Using Directive

When we insert a `using` statement into a scoping unit, we do not have to preface each member of the namespace with the name.  Most commonly we do this with the standard namespace.

```c++
#include <iostream>
using namespace std;

int main() {
   
    cout<<"Now we don't need the prefix\n";
    
    return 0;
}
```

In this case, we declare that we are using namespace `std` at a global scope through the rest of the file.  We can also use namespaces within a scoping unit, and that persists only within that unit.

Another form of the directive limits the reference to one item only.
The namespace must have been defined before we can do this.

{{< code-download file="/courses/cpp-introduction/codes/namespacescope.cxx" lang="c++" >}}

#### Using in Header Files

Introducing a `using` directive in a header file will bring the namespace into scope and can result in name collisions, especially if multiple headers are used.
It is strongly recommended that programmers use fully-qualified names in headers, with the `using` in any corresponding implementation files.
