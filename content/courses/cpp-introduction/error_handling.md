---
date: "2021-06-23"
title: "Error Handling"
weight: 120
---

Errors in code can be the result of a programmer's mistake (i.e. a bug) or they can occur at runtime due to invalid input, system errors, and so forth.  For debugging, C++ uses the `assert` statement.  For runtime errors, we can `try` something and handle the result if a problem occurs.

## Exceptions

Runtime errors may include
* An attempt to read a file that isn't present 
* A floating-point exception due to invalid input to a function
* A divide-by-zero floating-point exception
and so forth.

All these conditions (and many more) are called __exceptions__.  If the programmer does not handle them, execution will stop  

### Catching Exceptions

The "dangerous" section of code is enclosed in a _try block_.  If an error occurs within it, an exception will be _thrown_ so we can _catch_ it. 

```c++
try {
  if (y != 0) {
    float z=x/y;
  } else {
    throw 10;
  }
}
catch (int e) {
    cout << "An exception occurred, error "<<e<<"\n";
}
```

Exceptions can be stacked
```c+++
try {
   //code, throw things
}

catch (int e) { cout << "Integer exception \n"; }
catch (char c){ cout << "Character exception \n";}
}
```

### Generic Exceptions

If we do not know what type of exception might occur, we can replace the parameters to `catch` with an ellipsis `...`
```c++
try {
    if (y != 0) {
        float z=x/y;
    }
    else {
        throw "Exception";
    }
}
catch (...) {
   cout << "An exception occurred\n";
}
```

### Standard Exceptions

The C++ standard includes a base class for exceptions.  The `exception` header must be included. The programmer can derive custom exceptions, or use the built-in set provided by this class.  Some of the more commonly-seen ones are:
{{< table >}}
|   Exception     |    Cause    |
|-----------------|-------------|
| std::bad_alloc  | Can't allocate memory |   
| std::invalid_argument  | Invalid argument to function |
| std::length_error      | String too long |
| std::out_of_range      | Can be thrown by at operator (vector etc.) |
| std::overflow_error    | Numerical overflow error |
| std::underflow_error    | Numerical underflow error |
{{< /table >}}

See the [documentation](https://en.cppreference.com/w/cpp/error/exception) for more information.

```c++
try {
   cout << "Last item "<<v.at(6)<<endl;
}
   catch (const out_of_range &e) {
   cout << "Out of range \n";
}
```

**Exercise**
Assemble the example snippets into a working program.

{{< spoiler text="Example" >}}
{{< code-download file="/courses/cpp-introduction/solns/exceptions.cxx" lang="c++" >}}
{{< /spoiler >}}

## Assert

The `assert` statement is used to check for conditions that should not happen at all.  It is primarily used for debugging or quality assurance, not routine error handling.  The assertions in a program are typically disabled for production versions.

```c++
#include <cassert>

<code>
assert(conditional);
```

**Example**

```c++
assert(x != 0);
```

To disable assert statements, use the `NDEBUG` flag.
```c++
#include <cassert>
#define NDEBUG
```

A common practice is to define NDEBUG for debugging, then comment it out and recompile for the production executable.

### Static Assertions (C++11)

The `assert` statement is executed at runtime.  C++11 introduces the `static_assert` statement, which is evaluated at compile time.  
```c++
static_assert (const_bool);
```

One example of how static_assert may be helpful is checking for the size of certain types.  For instance, the C++ standard does not require a specific size for `int`, just a minimum.  The exact size may be platform-dependent.  The statement
```c++
sizeof(type) * CHAR_BIT
```
evaluates to the number of bits in `type` on the compiling platform.  If, for instance, we need to be sure that `int` is 32 bits, we can use `static_assert`
```c++
static_assert(sizeof(int) * CHAR_BIT == 32);
```
The `CHAR_BIT` macro is in the C `limits.h` header.

**Exercise**

Run the following code as is on your platform. Change the number of bits to make the assertion pass or fail.

{{< spoiler text="Example" >}}
{{< code-download file="/courses/cpp-introduction/codes/static_assert.cxx" lang="c++" >}}
{{< /spoiler >}}
