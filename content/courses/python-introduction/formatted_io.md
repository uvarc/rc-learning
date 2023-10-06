---
title: Formatted Output
toc: true
type: docs
draft: false
weight: 72

menu:
    python-introduction:
        parent: IO and Exceptions
---

So far we have only considered _list-directed_ input/output.  We leave it to the interpreter to format.  However, for output we often want or need more control over the appearance.  

## Format Codes

We define __format codes__ to describe how we wish the output to appear when printed.  A format code contains a letter for the type and, for numbers, one or more numerals to indicate the length to be printed.

### Floating Point

Formats for floating-point numbers are indicated by a pattern:
```python
F.wf
F.wg
F.we
```
F is the field width (total number of columns occupied), w is the number of digits to the right of the decimal point; these must be substituted by numbers. The letters f, g, and e are called _format codes_ and indicate floating-point (decimal point) format, general format (interpreter decides f or e), and exponential notation.  If we do not specify w for numbers, the default is 6 decimal digits printed.  If we specify the number of digits the interpreter will _round_ the result; if unspecified it will _truncate_.

Examples:

* Use exactly 15 spaces with 8 decimal places 
  * `15.8f`
* Let the interpreter decide the total field width but use 8 decimal places 
  * `.8f`
* Print a float as an integer (this will truncate, not round, the decimal places)
  * `0f`
* Print in scientific notation with three decimal places
  * `.3e`

### Strings 

Strings are specified with the letter `s`. They do not take a field width but can be adjusted or zero padded by using more advanced operations on the string to be printed.

### Integers

Integers are specified by `d` (this stands for decimal, as in base-10 number) with an optional field width.  If the field width is wider than the integer, it will be padded with blanks by default.  If it is narrower it will be ignored.  If omitted the interpreter will use the width of the integer.  

To pad with zeros, write the zero in front of the width specifier.

Examples:

```python
5d
05d
```

## Formatters

To construct our formatted output we use _format strings_, also called _formatters_.  We insert curly braces as placeholders for variables in the finished string.  Format codes go inside the braces following a colon.
```python
print("The value of pi is approximately {:.6f}".format(math.pi))
print("Pi to {:d} digits is {:.12f}".format(n,math.pi)
```

We have left the space to the left of the colon blank, but it represents the order of the arguments to `format`.  Thus the second example is equivalent to
```python
print("Pi to {0:d} digits is {1:.12f}".format(n,math.pi))
```

This means we can rearrange the output if we wish.
```python
print("Pi is {1:.12f} to {0:d} digits".format(n,math.pi))
```

Empty curly braces result in default formatting and ordering.  In this situation one could use list-directed formatting as well, but using a formatter enables more control over layout and spacing.
```python
print("I have {} books to read by {}".format(12,"tomorrow"))
```

Values can be aligned in a field with `<` (left align), `>` (right align), or `^` (center align). These symbols must be followed by a field width.
```python
print("The number {:^8} is {:>15.4f}".format("pi",math.pi))
```

**Exercise**

Type at the interpreter 

```python
import math
```

Practice printing math.pi
- Print without any formatting
- Print using the default f format
- Print to 5 decimal places 
- Print the integer part 
- For at least one of the above, add a message
- Print the number of digits to at least 6 spaces and pad with zeros
- Print the number with a message that specifies the number of digits, where the number of digits is also to be printed.

Even more sophisticated formatting is possible.  In the above examples, when printing pi the value of `n` had to be 12 or the output would be inconsistent.  In newer Python versions we can make the width a variable.

* print("Pi to {} digits is .{dec}f}".format(math.pi,dec=n))

## Formatting with f-Strings

For Python 3.6 and up the _formatted string literal_ or "f-string" was introduced.  These can be used to create formatted output easily.

**Example**

```python
n=12
print(f"Pi to {n} places is {math.pi:.{n}f}")
```

The f-string evaluates the contents of the curly braces when the program is run.  That means that expressions can be used inside the curly braces.

```python
print(f"Pi to {n//3} places is {math.pi:.{n//3}f}")
```

In this example, the integer division is required because the result of the expression must be an integer.

If quotation marks are needed inside an f-string (such as for a dictionary key) they must be single quotes.  In addition, the backslash `\` cannot be a character within an f-string, so if a character such as the newline `\n` is needed, it must be assigned to a variable.

## Resources

Full documentation is [here](https://docs.python.org/3/tutorial/inputoutput.html).  Details on format strings is [here](https://docs.python.org/3/library/string.html).

A good reference for f-strings is [here](https://zetcode.com/python/fstring/).
