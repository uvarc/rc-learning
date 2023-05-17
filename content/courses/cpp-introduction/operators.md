---
title: "Operators"
toc: true
type: book
weight: 34

---

Operators are characters or groupings of characters that take some number of variables or literals as _operands_, apply some specific mathematical, logical, or translational operation, and return a result.  Operators are defined by each programming language, although basic ones are often the same or similar.  The majority are mathematically binary operators, i.e. they take two operands, though nearly all languages have some unitary operators and a few have operators that take three or more operands.  Each operand must be of the specific types for which an operator is valid.

## Arithmetic Operations

Arithmetic operators are defined on integers, floats, and doubles.

`+ -` add, subtract

`* / `multiply, divide

Operators are applied in a particular order.  This is called precedence.
First (equal status):  * /
Second (equal status):  + -

Evaluation is left to right by precedence unless parentheses are used to group operators and operands.
The mnemonic *PEMDAS* is sometimes applied--*P*arentheses*E*xponents*M*ultiplication*D*ivision*A*ddition*S*ubtraction--but remember that MD and AS are equals within their ranking.  C++ does not provide an exponential operator so more correctly it would be *PMDAS*.  Exponentiation is supplied by the `pow` built-in function.
```c++
pow(x,3)
```
Even if the exponent is an integer, `pow` evaluates it as if both are floating-pint numbers.

### Special Considerations for Integer Operators

In C++ 2/3 is always zero!  Why?
Because 2 and 3 are both integers, so `/` is an integer operation that yields an integer result

Exercise:
What is 9/5?

The remainder can be obtained with the `%` operator. 
```c++
k=n%m
```
It is defined as
$$  n-floor(n/m) x m $$
It is mathematically well-defined for negative integers, but the results for such arguments are not generally what most programmers expect.  

Example:
{{< code-download file="/courses/cpp-introduction/codes/testmod.cxx" lang="c++" >}}

## Assignment Operators

The simple assignment of a value to a variable is through the equals sign `=`.
```c++
a=b;
```

C++ supports several compound assignment operators; the first operator specifies the arithmetic or other operation to be performed on the variable on the left-hand side, then the result is assigned back to the _same_ variable.
```c++
a+=b;
//Equivalent to
a=a+b;
```
There is no rule that assignment operators (aside from =) must be used but they save space and typing.

```c++
a+=b;
a-=b;
a*=b;
a/=b;
a%=b;
```
For the special case of adding or subtracting 1, special increment and decrement operators are defined.
```c++
++i;
--i;
i++;
i--;
```
Beware when assigning the result of an increment or decrement to another variable.  The "prefix" increment/decrement operators shown here first add or subtract, then change the value of the variable.  They are exactly equivalent to `i+=1` and `i-=1` respectively. The "post" operators `i++` and `i--` do not change the value of the variable before incrementing or decrementing.
{{< code file="/courses/cpp-introduction/codes/incdec.cxx" lang="c++" >}}
```no-highlight
 i is: 3
 j is: 2
 i is: 4
 j is: 4
```

**Exercise**
Run the following program.  Modify the values of the different variables and see what happens.
{{< code-download file="/courses/cpp-introduction/codes/expressions.cxx" lang="c++" >}}

## Conditional Operators

### Comparison Operators

These are used to compare numerical values.  They can also compare character and string variables; ordering is determined by the character encoding.  They return a Boolean value.

{{< table >}}
|   Symbols   |   Meaning  |
|-------------|------------|
|   ==        |  Equality  |
|   !=        | Not equal  |
|    <        | Less than  |
|    <=       | Less than or equal  |
|    >        | Greater than  |
|    >=       | Greater than or equal to  |
{{< /table >}}

### Boolean Operators

{{< table >}}
|   Operator    |   Meaning   |
|---------------|-------------|
|   !           |   Negation of what follows |
|   &&          |     and     |
|   ||          |     or      |
{{< /table >}}

It is important to note that `||` is an _inclusive_ or.  It evaluates to `true` if either operand is true.  This is different from many human languages, for which "or" is generally, though not always, _exclusive_.  An exclusive "or" is true only if exactly one of the conditions is true.
   You can have cake or ice cream (but not both).
An exclusive or can be constructed with
```c++
(a && !b) || ( a && b)
```
where `a` and `b` are Boolean expressions.  

"Truth tables" define the outcome of Boolean operators.  This example is for "and."

{{< table >}}
|   Operator    |   Operand 1   | Operand 2 |  Result |
|---------------|---------------|-----------|---------|
|     &&        |   true        |  true     |  true   |
|     &&        |   false       |  true     |  false  |
|     &&        |   true        |  false    |  false  |
|     &&        |   false       |  false    |  false  |
{{< /table >}}

## Conditional Operator Precedence

Like arithmetic operators, conditional operators have a precedence ordering.

* ! has the highest rank
* \>,>=,<,<= are equal and outrank == or !=
* ==,!= are equal and outrank &&
* && outranks ||

**Exercise**
Experiment with different values for the variables in this code.
{{< code-download file="/courses/cpp-introduction/codes/conditional_operators.cxx" lang="c++" >}}

## Bitwise Operators

Bitwise operators are defined for "integer type" variables, e.g. `int`, `short i
nt`, `unsigned int`, `char`, `unsigned char` etc.  They return another "integer type" variable.

{{< table >}}
|   Operator    |   Meaning   |
|---------------|-------------|
|   &           |     bitwise and     |
|   \|           |     bitwise or      |
|   ^           |     bitwise xor (exclusive or)      |
{{< /table >}}
