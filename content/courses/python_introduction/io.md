---
title: Input and Output
toc: true
type: book
draft: false
weight: 70
---

Programs are not very useful if they cannot communicate their results.  They are also not terribly useful if they cannot change their controlling parameters to compute different results.

It is best to make your program read its input parameters, rather than embedding them (hard-coding) them into the program body.  You may want to change the parameters later or run it for many different sets of parameters.  You may give your program to somebody else who will have to modify it.  Always assume your program may later be used for a slightly different purpose, whether by someone else or by you.

### Console Input

The console is a text interface for input to and output from the computer.  In Spyder, the iPython window itself can serve as a console.  In Jupyterlab the output console is indicated by lines marked with `Out []` whereas a reference to an input console will open a textbox. 

To read input from the console we use the `input()` function (Python 3+).  In Python 2.7 the equivalent is `raw_input()`.  Any string within the parentheses is optional and, if present, if will be printed to prompt the user.  The input from the user is captured as a string and returned; it must be stored for any subsequent use.

The input (raw_input) function returns _only_ a string.  If you need to use the values as any other type, you must perform the conversion yourself.
Example:

```python
weight=input("Enter your weight in pounds:")
print(type(weight))
weight=float(input("Enter your weight in pounds:"))
print(type(weight))
```

### Console Output

We have already been using the `print` function.  Let us now examine it in more detail.

The print function

* always inserts a space between its arguments
* always adds a newline character unless the `end` argument is added.
  * print(var,end="")

Messages can be added in between variables.

```python
h=1.45;
w=62.
print("Your BMI is",w/ht**2)
```

<details>
<summary>Exercise 15</summary>
<pre>
<p>
Use Spyder to write a <em>complete</em> program to compute BMI from weight and height input from a user.  First request the user's choice of units.  We have not spent much time with strings yet so you may use a digit to indicate the user's choice, but remember it will still be a string on input. Then request weight and height.  You will need to convert these from strings. Look up the correct conversion factors for Imperial to metric units. Compute the BMI and print the result to the console.   
</p>
</pre>
</p>
</details>

#### Formatted Output

So far we have only considered _list-directed_ input/output.  We leave it to the interpreter to format.  However, we often want or need more control over the format.  For this we define __format strings__.

Formats are indicated by a pattern

```python
F.wf
F.wg
F.we
```

F is the field width (total number of columns occupied), w is the number of digits to the right of the decimal point; these must be substituted by numbers. The letters f, g, and e are called _format codes_ and indicate floating-point (decimal point) format, general format (interpreter decides f or e), and exponential notation.  If we do not specify w for numbers, the default is 6 decimal digits printed.  If we specify the number of digits the interpreter will _round_ the result; if unspecified it will _truncate_.

Examples:

* Use exactly 15 spaces with 8 decimal places 
  * 15.8f
* Let the interpreter decide the total field width but use 8 decimal places 
  * .8f
* Print a float as an integer (this will truncate, not round, the decimal places)
  * 0f
* Print in scientific notation with three decimal places
  * .3e

Strings are specified with the letter `s`. They do not take a field width but can be adjusted or zero padded by using more advanced operations on the string to be printed.

Integers are specified by `d` (this stands for decimal, as in base-10 number) with an optional field width.  If the field width is wider than the integer, it will be padded with blanks by default.  If it is narrower it will be ignored.  If omitted the interpreter will use the width of the integer.  

To pad with zeros, write the zero in front of the width specifier.

Examples:

```python
5d
05d
```

### Formatters

To construct our formatted output we use _format strings_, also called _formatters_.  We insert curly braces as placeholders for variables in the finished string.  Format codes go inside the braces following a colon.

* print("The value of pi is approximately {:.6f}".format(math.pi))
* print("Pi to {:d} digits is {:.12f}".format(n,math.pi)

We have left the space to the left of the colon blank, but it represents the order of the arguments to `format`.  Thus the second example is equivalent to

* print("Pi to {0:d} digits is {1:.12f}".format(n,math.pi))

This means we can rearrange the output if we wish.

* print("Pi is {1:.12f} to {0:d} digits".format(n,math.pi))

Empty curly braces result in default formatting and ordering.  In this situation one could use list-directed formatting as well, but using a formatter enables more control over layout and spacing.

* print("I have {} books to read by {}".format(12,"tomorrow"))

<details>
<summary>Exercise 16</summary>

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

</details>

Even more sophisticated formatting is possible.  In the above examples, when printing pi the value of `n` had to be 12 or the output would be inconsistent.  In newer Python versions we can make the width a variable.

* print("Pi to {} digits is .{dec}f}".format(math.pi,dec=n))

#### Formatting with f-Strings

For Python 3.6 and up the _formatted string literal_ or f-string was introduced.  These can be used to create formatted output easily.

Example

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

### Resources

Full documentation is [here](https://docs.python.org/3/tutorial/inputoutput.html).  Details on format strings is [here](https://docs.python.org/3/library/string.html).

A good reference for f-strings is [here](https://zetcode.com/python/fstring/).
