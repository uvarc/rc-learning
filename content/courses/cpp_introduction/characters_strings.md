---
title: "Characters and Strings"
toc: true
type: book
weight: 23

---

## Character Variables

A `char` variable is 1 byte (8 bits).  It represents a single character.
```c++
char letter;
```

A `char` is unsigned.  C++ supports the `signed char` type but it is not as commonly used.  Since a signed char is effectively an 8-bit integer, it can be used in arithmetical expressions and some programmers who must write for devices with limited memory use the `char` type to save space.  A signed char can also be promoted to int consistently. 

Another use of `char` is to act as a `byte` type since C, and older C++, do not support `byte`.  Newer C++ standards (from C++17) and compilers that support them offer a type `std::byte`, but this is defined in terms of `unsigned char` and is a [class](/courses/cpp_introduction/classes), not a primitive type.
Byte types or their equivalents offer direct access to memory, which is organized by bytes.

### C-Style Strings

C-style strings are actually [arrays](/courses/cpp_introduction/arrays_vecs) of individual characters.  They must be declared with a fixed size, or allocated.
```c++
   char cstr[8];

   cstr="Hello";
```
The compiler automatically terminates each C-style string with an invisible "null" character.  If the assigned value does not fill the array, it will be padded with blanks.  It may not exceed the length of the string.  The size must account for the null terminating character, so
```c++
   char greeting[5]="Hello";
```
will result in an error, but 
```c++
   char greeting[6]="Hello";
```
works.

A C-style string may only be initialized to a quoted string when it is declared.```c++
   char greeting[6];
   greeting="Hello";
```
is invalid. 

### C Style Character Operations

C++ supports C-style string functions.  Include the `<cstring>` header.

|    Function    |      Operation    |   Usage     |
|--------------|-------------------|-------------|
|   strcpy      |  copy str2 to str1 |  strcpy(str1,str2)  |
|   strcat      |  concatenate str2 to str1|  strcat(str1,str2)  |
|   strcmp      |  compare two strings |  strcmp(str1,str2)  |
|   strlen      |  length of string (excludes null)  |  strlen(str)  |

Individual characters may be addressed using bracket notation.  Each character is one item, and the count begins from zero and goes to strlen-1.
```c++
   char greeting[8]="Hello";
   std::cout<<greeting[0]<<"\n";
   std::cout<<greeting[2]<<"\n";
```

Example:
{{< code-download file="/courses/cpp_introduction/codes/cstr.cxx" lang="c++" >}}

In the above code, pay attention to the lines
```c++
    std::cout<<strlen(greeting)<<"\n";
    std::cout<<strcat(greeting,musical_instr)<<"\n";
    std::cout<<greeting<<"\n";
    std::cout<<strlen(greeting)<<"\n";

    char str[6];
    strcpy(str,greeting);
    std::cout<<str<<"\n";
```
What result did this code yield?  On a Linux system with g++ the output was
```no-highlight
5
HelloCello
HelloCello
10
HelloCello
```
The size of `greeting` was doubled (not counting null terminators) even though it was declared size 6.  The compiler did not check for this.  The `strcpy` function then copied it to another variable of size 6.  
The result is a _buffer overflow_.  To see what might happen, run the following code
{{< code-download file="/courses/cpp_introduction/codes/buffer_oflow.cxx" lang="c++" >}}

On the same Linux system the result was
```no-highlight
Initial value of year: 2021
What happened to year? 1869376613
```

This occurred because `year` was declared right after `str`.  Str was only allocated 8 bytes of memory.  Nearly all compilers will place the next declared variable subsequent in memory, so in this example that was `year`.  Storing an excessively long variable into `str` caused it to overflow in memory and wipe out the value of `year`.

If using C-style strings and functions, guard against this by using

|    Function    |      Operation    |   Usage     |
|--------------|-------------------|-------------|
|   strncpy      |  copy str2 to str1, max n bytes of str2 |  strncpy(str1,str2,n)  |
|   strncat      |  concatenate str2 to str1, max n bytes of str2|  strncat(str1,str2,n)  |

One way to ensure that `n` is correct is to use `sizeof`, which returns a value in bytes.
```c++
strncpy(str1,str2,sizeof(str1)-1);
str1[strlen(str1)]='\0';
```
We must explicitly add the null character to the end of the target of the copy or even strncpy will overflow the buffer.

{{< code-download file="/courses/cpp_introduction/codes/buffer_oflow.cxx" lang="c++" >}}

The `strncat` function is more difficult to use correctly since it appends $n$ bytes from str2 regardless of the size of str1.  

Since we are programming in C++, not C, for most purposes it is better to use C++ strings (see [below](/courses/cpp_introduction/characters_strings#strings)),
which do not have these disadvantages.

## Character Sets and Encodings

Like everything else in the computer, characters must be represented by a sequence of 0s and 1s.  A catalogue of these representations is usually called an _encoding_.

The basic character set used by C++ is [**ASCII**](https://en.wikipedia.org/wiki/ASCII), the American Standard Code for Information Interchange. Internationally, it is sometimes known as US-ASCII.  Originally, 7 bits were used to represent data, resulting in a total of 128 (2<sup>7</sup>) available characters.  The first 32 are _non-printing_ characters that were mainly needed by the mechanical devices for which the encoding was developed.  Only a few non-printing characters are still used, among them line feed, carriage return, and even "bell."

{{< code file="/courses/cpp_introduction/codes/ringer.cxx" lang="c++" >}}

ASCII now uses 8 bits (extended ASCII) which doubles the number of available characters, but the first 128 character codes are the same as 7-bit ASCII.  It was not as well standardized as ASCII, though standards exist for Latin alphabets (ISO 8859-1, ISO Latin 1) and Cyrillic (ISO 8859-5).  Nearly all programming languages continue to restrict the characters allowed for statements to the original ASCII, even when larger character sets are the default and may be used for comments and output.
## Strings

A string is a sequence of characters of variable length.
Using strings requires a header:
```c++
#define <string.h>
```
The string is a _class_ , which is a little beyond our scope right now.  But we can still use basic functions without understanding the class.
```c++
string str, str1, str2;

str.size();  // length of string
str1+str2; // concatenate two strings
str.substr(2,5); // substring (counts from 0)
str[n];  // nth character (counts from 0), no checking
str.at(n);  // nth character (counts from 0), checks bounds for error
```

Many useful operations are available to work with strings.  Most of them have more options than presented here; see [documentation](https://en.cppreference.com/w/cpp/string/basic_string) for more details.

|    Method    |      Operation    |   Usage     |
|--------------|-------------------|-------------|
|   clear      |  delete all characters |  str.clear()  |
|   append      |  add characters to the end |  str.append(str1)  |
|   compare      |  compare two strings |  str1.compare(str2)  |
|   insert      |  add characters from position p  |  str.insert(p,str1)  |
|   replace      |  replace n characters from position p with str1 |  str.replace(p,n,str1)  |
|   find      |  find start position str1 in str  |  str.find(str2)  |
|   c_str      |  convert to C-style character array  |  str.c_str()  |

Example:

{{< code-download file="/courses/cpp_introduction/codes/strings.cxx" lang="c++" >}}

**Exercise**
In the above code, change "This" to "That" in `newtitle`.

### Strings, Wide Strings, and Unicode

Even extended ASCII accommodates far too few characters to accommodate more than a handful of alphabets, much less other writing systems.  [Unicode](https://en.wikipedia.org/wiki/Unicode) was created to address this.  The first 128 codes are still the 7-bit ASCII codes even with the millions available through Unicode.
The characters must still be _encoded_ and there are multiple standards for Unicode.  One of the most widely used is UTF-8, which has become a Web standard.  It is variable-width; characters are encoded in one to four bytes, with the first 128 characters one byte in size and corresponding exactly to ASCII.  The string class can support UTF-8 directly.  However, some software, especially on Windows, may use the UTF-16 encoding.  Windows uses the "wide string" or _wstring_t_ class for much of its character support; this makes cross-platform coding awkward. We will not go deeply into Unicode support but here is an example:

{{< code file="/courses/cpp_introduction/codes/unicode.cxx" lang="c++" >}}

Interested students can find discussions online of Unicode support.  Those who wish to write for Windows in particular may need to examine Microsoft-specific [documentation](https://docs.microsoft.com/en-us/archive/msdn-magazine/2016/september/c-unicode-encoding-conversions-with-stl-strings-and-win32-apis).
