---
title: "Console Input/Output"
toc: true
type: book
weight: 61

---

Most operating systems have some type of _console_.  Output written to the console appears as text on a terminal or equivalent.  Geany opens a window for console output when a program is executed.
We have been using console output via `std::cout` up till now without learning much about it.  It is associated with one of the _standard streams_ of C++ input/output.

## Streams

C++ handles input and output through a concept called _streams_.  A stream is an abstraction of a device (a keyboard, a monitor, in the past possibly a tape device, and so forth) to which characters can be printed or from which characters can be read.

A stream may be _buffered_; that is, the messages are accumulated in memory until the buffer is filled, when they are _flushed_ (written) to output.  

The standard streams are incorporated with the header
```c++
#include <iostream>
```
The iostream library is part of the C++ standard libraries set and contains _stream objects_ for standard streams.  They correspond to the standard streams of Unix or Mac OS.  Windows handles console IO differently, but iostream can map the stream objects to its equivalents.

| cin | standard input |
| cout | standard output |
| cerr | standard error |
| clog | standard logging |

The `cin` object is for input; it is a member of the _istream_ (input stream) class.  If a console is attached the others all output is to the same device (the screen).  They are members of the _ostream_ (output stream) class.  
Unix and Mac OS shells can redirect standard output and standard error to separate files.
Standard output is for "normal" output whereas standard error is used for error messages. 

On Linux cin and cout are buffered, whereas cerr is not.
Clog is attached to the standard-error stream but is buffered. 

The "c" is for _character_, which is the type these objects can handle.  For wide characters (two or more bytes) the streams `wcin`, `wcout`, `wcerr`, and `wclog` are available.

#### Output

```c++
#include <iostream>
  //write to standard output
  std::cout<<var1<<" "<<var2<<" "<<var3<<"\n"
  std::cout<<var1<<" "<<var2<<" "<<var3<<std::endl
```
The double less-than symbol `<<` is the _insertion operator_.  It inserts characters into the stream.  It is able to convert the values of variables into strings provided that how to do it is defined for the type of the variable.  Built-in types already provide those instructions; programmers can write those instructions for their own types.

The insertion operator will not print anything other than exactly what it is told to print.  It does not add any spaces between variables; they must be explicitly printed as in the example above.

The special symbol "\n" represents the "newline" character.  This terminates the current line and advances to the next line.  Cout, cerr, and clog  will not add a newline character unless told to do so.  Newline is just another character, so it is accumulated in the usual output buffer for the stream.  The `endl` manipulator also inserts a newline, but in addition it flushes the output buffer.  

C++ also supports the C `printf` function.  The `<cstdio>` header provides it.
{{< code file="/courses/cpp_introduction/codes/out_printf.cxx" lang="c++" >}}
More details are available at references such as [this](https://www.cplusplus.com/reference/cstdio/printf/).

#### Input

```c++
#include <iostream>
  //read from standard input
  std::cin>>var1>>var2
```
The double greater-than symbol `>>` is the _extraction operator_.  It "extracts" characters from the stream.  Similarly to the insertion operator, it can convert variables from characters to a type provided it is defined for that type.

The `cin` method reads variables separated by whitespace (spaces or tabs).
It will wait indefinitely until all variables specified are entered.  If the input is not of the correct type, it will fail silently and, depending on the compiler, may assign zeros or nonsense to subsequent variables.

**Exercise**

Try different input values for this code.  Try entering floats or words.
{{< code-download file="/courses/cpp_introduction/codes/read_cin.cxx" lang="c++" >}}

## Formatted Input/Output

Formatted input is rarely needed but can be used to control whitespace behavior, which is particularly useful for character (`char`) input.
Some documentation is [here](https://www.cplusplus.com/reference/ios/skipws/)

Formatted output permits greater control over the appearance of output.  Compilers tend to let their default output sprawl.

Formatted output also allows programmer control over the number of decimal digits printed for floating-point numbers.

### Manipulators

C++ uses _manipulators_ to modify the output of the stream methods cin and cout.
They are introduced with the `iomanip` header.
```c++
#include <iomanip>
```
A few basic manipulators:
* Output
   *  `endl` //flushes the output and inserts newline
   *  `ends` //outputs null character (C string terminator)
   *  `boolalpha` // true/false printed for Booleans
   *  `left`/`right`/`internal` //left/right/internal for fillers.
* Input
  * `ws` // reads and ignores whitespace
  * `skipws`/`noskipws` //ignore/read initial whitespace as characters (skip is the default)

These manipulators stay in effect in a given output stream until cancelled.

* `setw(n)` //Set width the output quantity will occupy
* `setprecision(n)` //Set number of places printed for floating-point numbers
* `fixed`/`scientific` //Fixed-point format or scientific notation format
* `setfill(c)` //Set a filler character `c`
* `setbase(n)` //Output in base `n` (options are 8, 10, or 16, or 0 which reverts to the default of decimal).

**Example**
{{< code-download file="/courses/cpp_introduction/codes/out_manip.cxx" lang="c++" >}}

A more complete list of manipulators is [here](https://www.cplusplus.com/reference/library/manipulators/).

**Exercises**

1. Write a program that computes pi using a trig identity such `asp=4\*atan(1)`. Remember
* #include <cmath>
* Using double precision, print pi in
  * Scientific notation
  * Scientific notation with 8 decimal places

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/cpp_introduction/solns/print_pi.cxx" lang="c++" >}}
{{< /spoiler >}}

2. In an “infinite” while loop:

Request an integer from the user without advancing to a new line, e.g.

“Please enter an integer:” <then read integer>

If the integer is 1, print “zebra”.  If it is 2, print “kangaroo”.  If it is anything else except for zero, print “not found”.  If it is 0, exit the loop.

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/cpp_introduction/solns/kangaroo.cxx" lang="c++" >}}
{{< /spoiler >}}
