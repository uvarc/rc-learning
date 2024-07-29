---
date: "2021-06-23"
title: "Variable Declarations"
weight: 22
---

Like most compiled languages, C++ is _statically_  _typed_ .  All variables must be _declared_ to be of a specific type before they can be used.  A variable’s type cannot be changed once it is declared.

C++ is (nearly) strongly typed.  Mixed-mode expressions are limited and many conversions must be explicit.

Variables are declared by indicating the type followed by a comma-separated list of variables followed by a semicolon.

```c++
int i, j, k;
float  x, y;
```

## Initializing at Compile Time

Variables can be declared and initialized at the same time:

```c++
float x=1.e.8, y=42.;
int i, j, k, counter=0;
```

C++, but not C, permits two additional formats for initialization.  _Constructor initialization_ encloses the initial value in parentheses.  _Uniform initialization_ uses curly braces.
```c++
float x(0.);
int   i{0};
int   j=2;
```

**Example**

Start Geany or your preferred editor or IDE.  Type
{{< code file="/courses/cpp-introduction/codes/variables.cxx" lang="c++" >}}

Variables can be declared and initialized anywhere in the body of the code as long as they are declared before they are used.  There are some implications for variable _scope_, which we will discuss [later](/courses/cpp-introduction/scope.md).  

{{< code file="/courses/cpp-introduction/codes/vardecls.cxx" lang="c++" >}}

### Auto

A recent introduction to the C++ (since C++11) standard is the `auto` declaration.  We have learned that the compiler is able to infer the type of literals.  Thus, it can deduce the type of variables that are initialized to literal values.  Moreover, if a variable is initialized to a variable previously declared to be a named type, the compiler can assign the same type to the new variable.
```c++
float x=12;
auto  y=x;

auto i=12;
```
Any `auto` variable _must_ be initialized.
```c++
auto z;
```
```no-highlight
error: declaration of ‘auto z’ has no initializer
```
If initializing an `auto` variable with a literal, be sure the type of the literal is what you intend.  Keep in mind that `3` and `3.0` are _distinct_ types to the compiler.  The `typeid` built-in will return the type.

{{< code file="/courses/cpp-introduction/codes/autos.cxx" lang="c++" >}}

On Unix with g++ this results in
```no-highlight
f d i
```
Recall that the literal `3.0` is a double.

If multiple assignments are made with `auto` they must be the same.
<br>
Wrong:
```
auto i=0, f=3.14159;
```
Right:
```
auto i=0;
auto f=3.14159;
```

### Decltype

Similar to auto, we can use the C++11 declaration `decltype` to declare that a variable is of the same type as another, previously-declared variable.  
```c++
float x;
decltype(x) y;
```
Variable `y` will take the same type as `x`.  This declaration was intended primarily for more advanced applications such as _lambda functions_ and templates, where it may be difficult to determine the type at compile time.

## Qualifiers

### Constants

Programmers can declare a variable to have a fixed value that cannot be changed. In C++ we add the qualifier `const` to these variables.
```
const double pi=3.141592653589793;
```
Variables declared `const` must be initialized when they are declared.  Any attempt to change them will result in a fatal compiler error.

### Volatiles

A _volatile_ variable is one that the compiler may not change and that may be beyond the control of the program itself.  The value of such a variable must always be read directly from main memory.  An example might be a program that obtains data from an instrument.  We do not want the compiler to modify the instrument readings so we would declare those variables volatile.

```c++
volatile float voltage;
``` 
