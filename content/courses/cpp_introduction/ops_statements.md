---
title: "Operators and Statements"
toc: true
type: book
weight: 24

menu:
    fortran_introduction:
        parent: Operators and Statements
        weight: 24

---

Operators are characters or groupings of characters that take some number of variables or literals as _operands_, apply some specific mathematical, logical, or translational operation, and return a result.  Operators are defined by each programming language, although basic ones are often the same or similar.  The majority are mathematically binary operators, i.e. they take two operands, though nearly all languages have some unitary operators and a few have operators that take three or more operands.  Each operand must be of the specific types for which an operator is valid.

## Arithmetic Operations

Arithmetic perators are defined on integers, floats, and doubles.

`+ -` add, subtract

`* / `multiply, divide

Operators are applied in a particular order.  This is called precedence.
First (equal status):  * /
Second (equal status):  + -

Evaluation is left to right by precedence unless parentheses are used to group operators and operands.
The mnemonic *PEMDAS* is sometimes applied--*P*arentheses*E*xponents*M*ultiplication*D*ivision*A*ddition*S*ubtraction--but remember that MD and AS are equals within their ranking.  C++ does not provide an exponential operator so more correctly it would be *PMDAS*.  Exponentiation is supplied by the`pow` built-in function.
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
{{< code-download file="/courses/cpp_introduction/codes/testmod.cxx" lang="c++" >}}

## Expressions and Statements

An _expression_ is a combination of variables, operators, and function invocations that can result in a unique evaluation.

C++ expressions are much like those of other languages.
```c++
a+3*c
8.d0*real(i,dp)+v**3
sqrt(abs(a-b))
A || B
myfunc(x,y)
```

If expressions are the "words," then _statements_ are the "sentences" of a programming language.  A statement is a combination of expressions and operators such as assignment (`=` in most languages) which describes a command to the compiler.
Statements in C++ terminate with a semicolon `;`
```c++
w=a+3*c;
z=myfunc(x,y);
if (A||B) C=0.; 
```
The length of a statement is in principle unlimited, but in practice it is limited by the compiler and the ability of humans to follow a sequence of symbols.  Always keep in mind that the compiler is never confused, but humans are often confused.

### Comments
One of the most important types of statement is the _comment_.  A comment is ignored by the compiler. C++ supports two types, C-style comments and C++ comments.

C-style comments are usually used for _comment blocks_ in C++.  The first line must start with `/*` and the comment continues until the compiler reaches `*/`.
Whitespace and line breaks are included in the comment.
```c++
/* This code prints "hello."
   Author:  A. Programmer
   Date:  Today
*/
```
The C++ comment starts with `//`.  It may begin anywhere on the line.  Anything following `//` is ignored up to the end of the line.
```c++
// A comment
// Another line needs another marker
if (A==B) {  // Check it out
```

### Code Blocks

A _code block_ is a set of statements that are functionally equivalent to a single statement.  For example, the language specifies that the _if_ construct is followed by a statement.  But it is rare that one statement suffices for the algorithm, so a code block must be used.
In C++, blocks are enclosed in curly braces `{}`.
Two choices for layout are widely used.  The first is "same line" where the opening curly brace is placed on the same line as the introductory clause.  The closing curly brace then aligns with the first letter of the statement.
```c++
    if ( A || B ) {
        C=0.;
        D=1.;
    }
```
The second option is to place the opening curly brace on the line below
```c++
    if ( A || B ) 
    {
        C=0.;
        D=1.;
    }
```
The first style saves space and the closing curly brace aligns visually with the code construct to which it belongs.  In the second style, the opening and closing curly braces are aligned.  Unless one style or the other is imposed by a particular coding project or employer, programmers should choose one and be consistent.

Blocks also have important implications for [scope](/courses/cpp_introduction/scope.md).
