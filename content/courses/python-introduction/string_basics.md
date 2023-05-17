---
title: Strings in Python
toc: true
type: docs
draft: false
weight: 51

menu:
    python-introduction:
        parent: Strings
        weight: 51
---

String literals are indicated by double quotes `"a"`.  Unlike some other languages, Python is not too picky about single or double quotes to indicate strings, but double quotes are usually preferred for multicharacter strings. If a string contains an apostrophe or its own quotes, the surrounding quotes must be of the other type.

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

## String Comparisons

String comparisons use the same familiar symbols as arithmetic comparisons, but with _lexical_ ordering.  This can result in some surprises if the strings represent numbers.  Never forget that strings are a completely different type from the numbers they may seem to represent!  Equality also requires exact equality, including spaces, matching cases, etc. 

* Equality 
  * `==`
* Lexically greater than or lexically greater than or equal
  * `\> \>=`
* Lexically less than or lexically less than or equal 
  * `< <=`

Example

```python
s1="This is a string."
s2="That is a string."
s3="This is a string"  #no period
print(s1==s3)
print(s1<=s2)
```

**Exercise**

```python
number_1="10"
number_2="2"
print(number_1 < number_2)
```

## String Operators and Functions

Python supplies many string operators and functions.  Among the most commonly used are

* concatenation
  * `s1 + s2`
* number of characters
  * `len(string)`
* type conversion from numerical type to string
  * `str(f)`
* type conversion from string to numerical type.  This must be possible according to the interpreter's rules for the numbers.  In particular, the string `"3."` does not represent an integer.
  * `float(s)`
* raw string: no characters are taken to be special characters.  Sometimes particularly useful on Windows. Either `r` or `R` can be used.
  * `r'This is a string \ with no special characters \n'`

**Exercise**

Type in the following code.  What causes the difference?

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

## Substrings

Although a particular string variable is immutable, it is possible to extract substrings from it.

```python
sub_string=string[0:3]
```

In this context the colon (:) is called the _range operator_.  For all ordered types, Python counts _from zero_.  So the first character is numbered 0, the second is 1, and so forth.  As we have seen before, the upper bound is always _exclusive_ in Python. Thus the variable `sub_string` consists of characters 0, 1, and 2.

Since strings are immutable we cannot assign values to a substring; that is, they cannot appear on the left-hand sign of an assignment = statement.

**Exercise**

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
