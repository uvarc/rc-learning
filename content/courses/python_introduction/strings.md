---
title: Strings
toc: true
type: docs
draft: false
weight: 5
menu:
  python_introduction:
    parent: Introduction to Programming in Python
    weight: 5
---

The string type is widely used in Python.  A __string__ consists of a sequence of characters, even if the sequence length is 1--Python does not make a distinction between a character and a one-character string.  The string is a compound type and immutable.  The representation of a single character internallly in the computer as a sequence of bits is called the _encoding_.  Individual characters are represented either by the ASCII standard (1 byte per character) or Unicode (2-4 bytes per character).  Strings that are to be treated as Unicode are type `unicode` rather than string, but otherwise behave similarly.  The default encoding may depend on the operating system but in newer Python versions is usually a standard called _utf-8_.  UTF-8 can represent over one hundred thousand characters and can embed different scripts within the same text file.

String literals are indicated by double quotes `"a"`.  Unlike some other languages, Python is not too picky about single or double quotes, but double quotes are preferred for multicharacter strings. If a string contains an apostrophe or its own quotes, the surrounding quotes must be of the other type.

```python
s1="This is a string."
s2="It's time to go."
s3='The man said, "Time to go."'
```

The length of a string can be dynamically determined when the script is run, but once set, it is fixed because strings are immutable. The string variable can be overwritten, however.

```python
Line_1="The first line of a file\n"
```

The `\n` symbol represents a new line and is treated as a single character.  The length of the above string is 25; spaces and the newline count.

If a string literal is surrounded by triple double quotes `"""s"""` it is verbatim, including newlines typed.

```python
s="""This string is a
     multiline quote."""
```

If evaluated as an expression, the string will show the newline.  If the print function is used, it will print exactly as typed.

### String Operators

Python supplies many string operators and functions.  Among the most commonly used are

* concatenation
  * s1 + s2
* number of characters
  * len(string)
* type conversion from numerical type to string
  * str(f)
* type conversion from string to numerical type.  This must be possible according to the interpreter's rules for the numbers.  In particular, the string `"3."` does not represent an integer.
  * float(s)
* raw string: no characters are taken to be special characters.  Sometimes particularly useful on Windows. Either `r` or `R` can be used.
  * `r'This is a string \ with no special characters \n'`

<details>
<summary>Exercise 9</summary>

```python
s1="Today \n is a new day."
s2=r"Today \n is a new day."
print(s1)
print(s2)
```

Define variables `x=21.0`, `n=30`, `s="My new string."`
Convert `n` into a float and store the results into a new variable `y`.
Set a variable `the_answer` containing the literal string "42." (be sure to include the period). Type

```python
z=int(the_answer)
```

What happened? Try

```python
z=float(the_answer)
```

</details>

#### String Comparison Operators

String comparisons use the familiar symbols but _lexical_ ordering.  This can result in some surprises if the strings represent numbers.  Never forget that strings are a completely different type from the numbers they may seem to represent!  Equality also requires exact equality, including spaces, matching cases, etc. 

* Equality 
  * ==
* Lexically greater than or lexically greater than or equal
  * \> \>=
* Lexically less than or lexically less than or equal 
  * < <=

Example

```python
s1="This is a string."
s2="That is a string."
s3="This is a string"  #no period
print(s1==s3)
print(s1<=s2)
```

<details>
<summary>Exercise 10</summary>

```python
number_1="10"
number_2="2"
print(number_1 < number_2)
```

</details>

#### Substrings

Although a particular string variable is immutable, it is possible to extract substrings from it.

```python
sub_string=string[0:3]
```

In this context the colon (:) is called the _range operator_.  For all ordered types, Python counts _from zero_.  So the first character is numbered 0, the second is 1, and so forth.  The upper bound is always _exclusive_ in Python. Thus the sub_string consists of characters 0, 1, and 2.

Since strings are immutable we cannot assign values to a substring; that is, they cannot appear on the left-hand sign of an assignment = statement.

<details>
<summary>Exercise 11</summary>
Type into the Spyder interpreter pane or a JupyterLab notebook.  Remember that in Jupyter each evaluation expression should be run in its own cell.

```python
title="This is a string."
subtitle="This is another string."
len(title)
title+":"+subtitle
newtitle=title+" : "+subtitle
len(newtitle)
newtitle[2:4]="at"  #Error-why?
x=19.58
print("The value of x is {:f}".format(x))
```

</details>

### More Advanced String Handling

One of Python's strong points is its ability to do many things well, so both numerical and textual analysis can be done with the same language.  We will look at some of the many ways we can manipulate strings in Python.  It is important to distinguish between string _functions_, which take a string as an argument, e.g. `sfunc(mystring)`, and string _methods_ that follow the string variable, e.g. `mystr.amethod()`.  Both perform operations on the string.  Later, when we have studied classes, we will better understand why the syntax differs.

#### Categorization

Several methods are available to determine whether a string represents letters or may be text.  They apply to the entire string.

* isalpha, isdigit, isalnum
  * is alphabetic, is a number, is alphanumeric (combination of numbers and other characters)
    * mystr.isalpha()
* isupper, islower, istitle
  * is uppercase, is lowercase, is "title case" (first letter of each word capitalized, all others lower case)
    * mystr.isupper()

#### Manipulating Case

* Switch to all uppercase
  * upper
    * mystr.upper()
* Switch to all lowercase
  * lower 
    * mystr.lower()
* Convert to title case 
  * title 
    * mystr.title()
* Swap cases 
  * swapcase
    * mystr.swapcase()

#### Searching and Tests

* Find a character or substring.  Returns location of _first_ occurrence only.
  * find
    * returns -1 if it does not find the substring 
    * mystr.find(s)
  * rfind(s)
    * searches right to left
* index
  * throws an exception if the substring is not found 
    * mystr.index(s)
  * rindex(s)
    * searches right to left
* count
  * Counts the number of occurrences of substring s.  Case sensitive.
    * mystr.count(s)
* endswith
  * Determines whether a string ends with a particular substring
    * mystr.endswith(s)
* startswith
  * Determines whether a string starts with a particular substring
    * mystr.startswith(s)

#### Modifying and Filling

* Remove characters from beginning and end (empty parentheses remove spaces and tabs). The angle brackets indicate an option and are not typed out.
  * mystr.strip(<chars>)
    * mystr.rstrip(<chars>), string.lstrip(<chars>)
* Replace substring a with b
  * mystr.replace(a,b)
* Expand tabs 
  * The default is 8 spaces per tab.  If a different number is required, pass it in the parentheses.
  * mystr.expandtabs()  #8 spaces 
  * mystr.expandtabs(4) #4 spaces 
* Justify in a field of width n spaces 
  * mystr.rjust(n), mystr.ljust(n)
* Center in a field of n spaces 
  * mystr.center(n)
* Fill spaces with zeros in field of width n (mainly used for numbers)
  * mystr.zfill(n)

#### Splitting and Joining

* Split on string `s`.  Most usually splits on a character.  Splits on whitespace (spaces and tabs) when the delimiter isn't specified.  Returns a list with the delimiter removed, and each separated string an element of the list.
  * split(\<s>)
    * mystr.split()
    * mystr.split(\',\')
* Split on newlines.  Returns a list of the lines, with newline characters stripped.
  * mystr.splitlines()
* Join a list of strings with a string (usually a single character).  This is the inverse of split.  The syntax is peculiar, for Python.
  * \<s>.join(list)
    * \"\".join(strlist)
      * joins a list with no spaces or other characters between
    * \",\".join(strlist)
      * joins a list with commas between

#### String Module

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

### Resources

The official documentation for the string type is [here](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str).  A more complete discussion of built-ins for strings is [here](https://docs.python.org/3/library/stdtypes.html#string-methods), including optional arguments for some of the methods described above.  
