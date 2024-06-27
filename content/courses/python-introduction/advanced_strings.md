---
title: Advanced String Handling
toc: true
type: docs
draft: false
weight: 63
date: "2020-11-17T00:00:00"

menu:
    python-introduction:
        parent: Strings
---

One of Python's strong points is its ability to do many things well, so both numerical and textual analysis can be done with the same language.  We will look at some of the many ways we can manipulate strings in Python.  It is important to distinguish between string _functions_, which take a string as an argument, e.g. `sfunc(mystring)`, and string _methods_ that follow the string variable, e.g. `mystr.amethod()`.  Both perform operations on the string.  The [classes](/courses/python-introduction/classes) chapter will make clear why the syntax differs.

The result of all the built-in methods and functions must be stored into a new string variable; they do not modify the original string.  For example,
```python
str_list=str.split(',')
```

### Categorization

Several methods are available to determine whether a string represents letters or may be text.  They apply to the entire string. They return a Boolean value.

* Is alphabetic, is a number, is alphanumeric (combination of numbers and other characters).  Whitespace is none of these.
  * `isalpha`, `isdigit`, `isalnum`
    * `mystr.isalpha()`
* Is uppercase, is lowercase, is "title case" (first letter of each word capitalized, all others lower case)
  * `isupper`, `islower`, `istitle`
    * ` mystr.isupper()`

#### Manipulating Case

* Switch to all uppercase
  * `upper`
    * `mystr.upper()`
* Switch to all lowercase
  * `lower`
    * `mystr.lower()`
* Capitalize first letter
  * `capitalize`
    * `mystr.capitalize()`
* Convert to title case 
  * `title`
    * `mystr.title()`
* Swap cases 
  * `swapcase`
    * `mystr.swapcase()`

### Searching and Tests

* Find a character or substring.  Returns location of the _first_ occurrence only.
  * `find`
    * returns `-1` if it does not find the substring 
    * `mystr.find(s)`
  * `rfind(s)`
    * searches right to left
* Find the index of the beginning (left to right) of a substring.  Throws an _exception_ (error) if the substring is not found.
  * `index`
    * `mystr.index(s)`
  * `rindex(s)`
    * searches right to left
* Count the number of occurrences of substring `s`.  It is case sensitive.
  * `count`
    * `mystr.count(s)`
* Determine whether a string ends with a particular substring
  * `endswith`
    * `mystr.endswith(s)`
* Determines whether a string starts with a particular substring
  * `startswith`
    * `mystr.startswith(s)`

### Modifying and Filling

* Remove characters from the beginning and end (if no arguments, i.e. the parenteses are left empty, remove spaces and tabs). The angle brackets indicate an option that can be omitted and are not typed out.
  * `mystr.strip(<chars>)`
    * `mystr.rstrip(<chars>)`, `string.lstrip(<chars>)`
    * The default is _whitespace_ (spaces and tabs)
* Replace substring `a` with `b`.
  * `mystr.replace(a,b)`
* Expand tabs.
  * The default is 8 spaces per tab.  If a different number is required, pass it in the parentheses.
  * `mystr.expandtabs()`  #8 spaces 
  * `mystr.expandtabs(4)` #4 spaces 
* Justify in a field of width `n` spaces. Returns original string if the field width is too short.
  * `mystr.rjust(n)` # right justify
  * `mystr.ljust(n)` # left justify
* Center in a field of `n` spaces. Returns original string if the field width is too short.
  * `mystr.center(n)`
* Fill spaces with zeros in field of width `n` (mainly used with numbers).
  * `mystr.zfill(n)`

### Splitting and Joining

* Split on string `s`.  Is mostly used to split on a character or sometimes a very short string.  Splits on whitespace (spaces and tabs) when the delimiter isn't specified.  Returns a list with the delimiter removed, and each separated string an element of the list.
  * `split(<s>)`
    * `mystr.split()`
    * `mystr.split(',')`
* Split on newlines.  Returns a list of the lines, with newline characters stripped.
  * `mystr.splitlines()`
* Join a list of strings with a string (usually a single character) into a single string.  This is the inverse of split.  The syntax is peculiar, for Python.
  * `<s>.join(list)`
    * joins a list of strings with no spaces or other characters between them.
       * `"".join(strlist)`
    * joins a list with commas between
       * `",".join(strlist)`

### String Module

All of the string operators and methods are available in the base Python installation.  However, there is a package `string` which contains some useful string literals.

```python
import string 
string.ascii_letters
string.ascii_lowercase 
string.ascii_uppercase 
string.digits 
string.hexdigits 
string.octdigits 
string.punctuation   #(depends on the locale)
string.printable 
string.whitespace    #space, tab, linefeed, return, formfeed, and vertical tab.
```

**Exercise**

Assign the string "1,ninety,23.8,4,two" to a variable. Split the string on the commas. Go through the resulting list and find the items that consist of alphabetical characters, collecting them into another list.  Join them into a new string using `-` as the delimiter.
{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/exercises/string_functions.py" lang="python" >}}
{{< /spoiler >}}

### Resources

The official documentation for the string type is [here](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str).  A more complete discussion of built-ins for strings is [here](https://docs.python.org/3/library/stdtypes.html#string-methods), including optional arguments for some of the methods described above.  

