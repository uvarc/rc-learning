---
date : "2021-06-23T00:00:00-05:00"
title: "Expressions and Statements"
toc: true
type: book
weight: 24

---

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

Blocks also have important implications for [scope](/courses/cpp-introduction/scope.md).
