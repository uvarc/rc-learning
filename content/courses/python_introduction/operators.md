---
title: Operators
toc: true
type: book
draft: false
weight: 22
---

Operators are defined on types and return a new value, usually of the same but sometimes of a different type.  The most familiar are the arithmetic operators `+ - * /` (addition, subtraction, multiplication, division).  Python also provides an exponentiation operator `**`, e.g. `a**b`.  Python also accepts `pow(a,b)` for exponentiation.

The equal sign (=) is an _assignment operator_ in Python.  It does not indicate equality; rather it assigns a value to a variable.  Since variables represent locations in memory, the assignment tells the interpreter to place the value into that memory location.  This means that a mathematically nonsensical expression like

```python
x=x+1
```

is perfectly correct Python.  The interpreter fetches the value of x from memory, adds one to it, and stores the result back into the same memory location.

Python supports add/assign and similar operators.  Thus

```python
x+=1
```

is the same thing as

```python
x=x+1
```

Experienced programmers tend to use operator/assignment (+=, -=, \*=, /=) but it is not required.

## Integer Operators

The arithmetic operators are defined on all numerical types.  Two additional operators are generally or always used to return integers.

* Integer division `//` returns the integer part of the division.  For example, `7//2=3`. 
* The mod or modulo operator `%` returns the remainder of the division, so `7%2=1`.

These operators are also defined on floating-point numbers, but the results may not be what you expect!  Also avoid using `%` with negative numbers, for the same reason.

_Special note for Python 2.7 users_: In Python 2.7, `2/3=0`.  This is because each operand is an integer, so the operator returns an integer.  However, in Version 3 and up, `2/3=0.6666666666666666` (this value is the result of an expression evaluation).  Python 2.7 users who want the newer behavior should begin each file with
```python
from __future__ import division
```

If you'd like the new print function as well, modify this line to
```python
from __future__ import division, print_function
```

<details>
<summary>Exercise 1</summary>
Type or paste into your choice of Spyder's interpreter pane or a JupyterLab cell the following assignments.

{{< code-snippet >}}
x=17.
Xs=11.
num_1=10
num_2=14
{{< /code-snippet >}}

Type the following lines one at a time and examine the results. In JupyterLab each line must be in its own cell. In the Spyer interpreter pane it will evaluate each line when you go to the next one.

```python
x
Xs/x
Xs//x
Xs/x+x
Xs/(x+x)
x/num_1
num_1/num_2
num_2/num_1
```

</details>

<details>
<summary>Exercise 2</summary>

Use the same method of typing one line at a time (using a separate cell for each in Jupyter) to study the outcome of the following operations:

```python
4+2*3
(4+2)*3
20/4*5
20/(4*5)
.1+.2
5//2
5//-2
11//3
11.4//3.5
11%3
11.4%3.5 #?
11.4-(11.4//3.5)*3.5
```
</details>

## Boolean Operators

Boolean operators operate on Boolean expressions.  

* Negation
  * `not someVar`
* AND 
  * `someBool and anotherBool`
* OR
  * `someBool or anotherBool`
    * __Warning__: in Python `or` is _nonexclusive_.  The expression is True if _either_ or _both_ are True.
    * This is different from the usual meaning of _or_ in natural languages. "You can have ice cream or cake" usually implies "but not both."  But in Python,
      * `if ice_cream_OK or cake_OK:`
        is true if both are True.  

## Comparison Operators

Comparison operators operate on other types but return Boolean values.  They are also called _conditional_ operators or relational operators.

Comparison operators represent relationships between two or more variables.  They can be defined on any type, but arithmetic and string comparisons are the most common.

### Arithmetic Comparison Operators

* Equal, not equal. Note that equality is a double equals sign.
  * == !=
* Less than, greater than, less than or equal to, greater than or equal to
  * &lt; &gt;  &lt;=  &gt;=

_Chaining_

In Python we can write an expression like 
```python
0<a<=1
```
just as in the analogous mathematical expression.  An `and` operator is always assumed.  This chain is equivalent to
```python
0<a and a<=1
```

### String Comparison Operators

String comparison operators look the same as arithmetic comparison operators but use _lexical ordering_.  Strings are compared character by character.  Spaces are counted as well as some "nonprinting" characters.  Equality requires an _exact_ match.

{{< code-snippet >}}
str1="News of the world"
str2="News of the world  "
print(str1==str2)
{{< code-snippet >}}

{{< /code-snippet >}}
str3="uppercase"
str4="Uppercase"
print(str3>str4)
{{< /code-snippet >}}

Notice that uppercase letters are lexically less than lowercase letters.  This is due to the ordering sequence in the _character set_ being used.
