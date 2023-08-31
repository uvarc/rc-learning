---
title: Advanced String Handling
toc: true
type: docs
draft: false
weight: 53

menu:
    python-introduction:
        parent: Strings
---

One of Python's strong points is its ability to do many things well, so both numerical and textual analysis can be done with the same language.  We will look at some of the many ways we can manipulate strings in Python.  It is important to distinguish between string _functions_, which take a string as an argument, e.g. `sfunc(mystring)`, and string _methods_ that follow the string variable, e.g. `mystr.amethod()`.  Both perform operations on the string.  The [classes](/courses/python-introduction/classes) chapter will make clear why the syntax differs.

### Categorization

Several methods are available to determine whether a string represents letters or may be text.  They apply to the entire string.

* `isalpha`, `isdigit`, `isalnum`
  * is alphabetic, is a number, is alphanumeric (combination of numbers and other characters)
    * `mystr.isalpha()`
* `isupper`, `islower`, `istitle`
  * is uppercase, is lowercase, is "title case" (first letter of each word capitalized, all others lower case)
    *` mystr.isupper()`

#### Manipulating Case

* Switch to all uppercase
  * `upper`
    * `mystr.upper()`
* Switch to all lowercase
  * `lower`
    * `mystr.lower()`
* Convert to title case 
  * `title`
    * `mystr.title()`
* Swap cases 
  * `swapcase`
    * `mystr.swapcase()`

### Searching and Tests

* Find a character or substring.  Returns location of _first_ occurrence only.
  * `find`
    * returns -1 if it does not find the substring 
    * `mystr.find(s)`
  * `rfind(s)`
    * searches right to left
* `index`
  * throws an exception if the substring is not found 
    * `mystr.index(s)`
  * `rindex(s)`
    * searches right to left
* `count`
  * Counts the number of occurrences of substring s.  Case sensitive.
    * `mystr.count(s)`
* `endswith`
  * Determines whether a string ends with a particular substring
    * `mystr.endswith(s)`
* `startswith`
  * Determines whether a string starts with a particular substring
    * `mystr.startswith(s)`

### Modifying and Filling

* Remove characters from beginning and end (empty parentheses remove spaces and tabs). The angle brackets indicate an option that can be omitted and are not typed out.
  * `mystr.strip(<chars>)`
    * `mystr.rstrip(<chars>)`, `string.lstrip(<chars>)`
    * The default is _whitespace_ (spaces and tabs)
* Replace substring a with b
  * `mystr.replace(a,b)`
* Expand tabs 
  * The default is 8 spaces per tab.  If a different number is required, pass it in the parentheses.
  * `mystr.expandtabs()`  #8 spaces 
  * `mystr.expandtabs(4)` #4 spaces 
* Justify in a field of width n spaces 
  * `mystr.rjust(n)`, `mystr.ljust(n)`
* Center in a field of n spaces 
  * `mystr.center(n)`
* Fill spaces with zeros in field of width n (mainly used for numbers)
  * `mystr.zfill(n)`

### Splitting and Joining

* Split on string `s`.  Most usually splits on a character.  Splits on whitespace (spaces and tabs) when the delimiter isn't specified.  Returns a list with the delimiter removed, and each separated string an element of the list.
  * `split(<s>)`
    * `mystr.split()`
    * `mystr.split(',')`
* Split on newlines.  Returns a list of the lines, with newline characters stripped.
  * `mystr.splitlines()`
* Join a list of strings with a string (usually a single character).  This is the inverse of split.  The syntax is peculiar, for Python.
  * `<s>.join(list)`
    * joins a list with no spaces or other characters between
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

