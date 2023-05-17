---
title: "Encodings and Strings"
toc: true
type: book
weight: 24

---

Like everything else in the computer, characters must be represented by a sequence of 0s and 1s.  A catalogue of these representations is usually called an _encoding_.

The basic character set used by C++ is [**ASCII**](https://en.wikipedia.org/wiki/ASCII), the American Standard Code for Information Interchange. Internationally, it is sometimes known as US-ASCII.  Originally, 7 bits were used to represent data, resulting in a total of 128 (2<sup>7</sup>) available characters.  The first 32 are _non-printing_ characters that were mainly needed by the mechanical devices for which the encoding was developed.  Only a few non-printing characters are still used, among them line feed, carriage return, and even "bell."

{{< code file="/courses/cpp-introduction/codes/ringer.cxx" lang="c++" >}}

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

{{< table >}}
|    Method    |      Operation    |   Usage     |
|--------------|-------------------|-------------|
|   clear      |  delete all characters |  str.clear()  |
|   append      |  add characters to the end |  str.append(str1)  |
|   compare      |  compare two strings |  str1.compare(str2)  |
|   insert      |  add characters from position p  |  str.insert(p,str1)  |
|   replace      |  replace n characters from position p with str1 |  str.replace(p,n,str1)  |
|   find      |  find start position str1 in str  |  str.find(str2)  |
|   c_str      |  convert to C-style character array  |  str.c_str()  |
{{< /table >}}

Example:

{{< code-download file="/courses/cpp-introduction/codes/strings.cxx" lang="c++" >}}

**Exercise**
In the above code, change "This" to "That" in `newtitle`.
{{< code-download file="/courses/cpp-introduction/solns/strings.cxx" lang="c++" >}}

### Strings, Wide Strings, and Unicode

Even extended ASCII accommodates far too few characters to accommodate more than a handful of alphabets, much less other writing systems.  [Unicode](https://en.wikipedia.org/wiki/Unicode) was created to address this.  The first 128 codes are still the 7-bit ASCII codes even with the millions available through Unicode.
The characters must still be _encoded_ and there are multiple standards for Unicode.  One of the most widely used is UTF-8, which has become a Web standard.  It is variable-width; characters are encoded in one to four bytes, with the first 128 characters one byte in size and corresponding exactly to ASCII.  The string class can support UTF-8 directly.  However, some software, especially on Windows, may use the UTF-16 encoding.  Windows uses the "wide string" or _wstring_t_ class for much of its character support; this makes cross-platform coding awkward. We will not go deeply into Unicode support but here is an example:

{{< code file="/courses/cpp-introduction/codes/unicode.cxx" lang="c++" >}}

Interested students can find discussions online of Unicode support.  Those who wish to write for Windows in particular may need to examine Microsoft-specific [documentation](https://docs.microsoft.com/en-us/archive/msdn-magazine/2016/september/c-unicode-encoding-conversions-with-stl-strings-and-win32-apis).
