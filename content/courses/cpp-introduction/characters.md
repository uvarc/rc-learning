---
title: "Character Variables"
toc: true
type: book
weight: 23

---

A `char` variable is 1 byte (8 bits).  It represents a single character.
```c++
char letter;
```

A `char` is unsigned.  C++ supports the `signed char` type but it is not as commonly used.  Since a signed char is effectively an 8-bit integer, it can be used in arithmetical expressions and some programmers who must write for devices with limited memory use the `char` type to save space.  A signed char can also be promoted to int consistently. 

Another use of `char` is to act as a `byte` type since C, and older C++, do not support `byte`.  Newer C++ standards (from C++17) and compilers that support them offer a type `std::byte`, but this is defined in terms of `unsigned char` and is a [class](/courses/cpp-introduction/classes), not a primitive type.
Byte types or their equivalents offer direct access to memory, which is organized by bytes.

### C-Style Strings

C-style strings are actually [arrays](/courses/cpp-introduction/arrays_vecs) of individual characters.  They must be declared with a fixed size, or allocated.
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

A C-style string may only be initialized to a quoted string when it is declared.
```c++
   char greeting[6];
   greeting="Hello";
```
is invalid. 

### C Style Character Operations

C++ supports C-style string functions.  Include the `<cstring>` header.

{{< table >}}
|    Function   |      Operation    |   Usage     |
|:-------------:|:-----------------:|:-----------:|
|   strcpy      |  copy str2 to str1 |  strcpy(str1,str2)  |
|   strcat      |  concatenate str2 to str1|  strcat(str1,str2)  |
|   strcmp      |  compare two strings |  strcmp(str1,str2)  |
|   strlen      |  length of string (excludes null)  |  strlen(str)  |
{{< /table >}}

Individual characters may be addressed using bracket notation.  Each character is one item, and the count begins from zero and goes to strlen-1.
```c++
   char greeting[8]="Hello";
   std::cout<<greeting[0]<<"\n";
   std::cout<<greeting[2]<<"\n";
```

Example:
{{< code-download file="/courses/cpp-introduction/codes/cstr.cxx" lang="c++" >}}

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
The result is a _buffer overflow_.  

To see what can happen, compile and run the following code
{{< code-download file="/courses/cpp-introduction/codes/buffer_oflow.cxx" lang="c++" >}}

Type in a short user name (any string), then type `Eleventy` as your password. It should work as expected.  Now try typing a user name that is longer than 10 characters and see what happens.

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

The `strncat` function is more difficult to use correctly since it appends $n$ bytes from str2 regardless of the size of str1.  

In general, it is best to avoid fixed-size char variables as much as possible, because C++ (and C) does not check C-style array bounds. Similar problems can occur with numerical arrays, but in those cases the result is typical a segmentation fault. Buffer overflows in characters can result in insecure programs.

Since we are programming in C++, not C, for most purposes it is better to use C++ strings (see [here](/courses/cpp-introduction/encodings_strings#strings)),
which do not have these disadvantages.

