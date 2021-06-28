## Character Variables

A `char` variable is 1 byte (8 bits).  It represents a single character.
A character array has a fixed length that must be declared at compile time, or allocated later.
```c++
char[8] mychar;
```
The default length is 1 character.
```c++
char letter;
```

A char is unsigned.  C++ supports the `unsigned char` type but it is not as commonly used.
# Non-numeric Types: String

## Strings

A string is a sequence of characters of variable length.

Requires adding a header

#define <string.h>

The string is a _class_ , which is a little beyond our scope right now.  But you can still use basic functions without understanding the class.

stringstr, str1, str2;

str.size();  // length of string

str1+str2; // concatenate two strings

str.substr(2,5); // substring (counts from 0)

