---
title: "Conditionals"
toc: true
type: book
weight: 31

menu:
    cpp_introduction:
        parent: Conditionals
        weight: 31

---

A _conditional_ is a programming construct that implements decisions. 
* _If_ the weather is good _then_ we will go for a walk, _else_ we will stay inside and watch TV.  
* _If_ it is cold enough to snow I will wear my heavy coat, _else if_ it is warmer and just rains I will wear my rainjacket.
The expression following each _if_ or _else_ must be true or false, i.e. a _logical_ expression (in Fortran terminology).

## Conditional Operators

### Comparison Operators

These are used to compare numerical values.  They can also compare character and string variables; ordering is determined by the character encoding.  They return a Boolean value.  

|   Symbols   |   Meaning  |
|-------------|------------|
|   ==        |  Equality  |
|   !=        | Not equal  |
|    <        | Less than  |
|    <=       | Less than or equal  |
|    >        | Greater than  |
|    >=       | Greater than or equal to  |

### Boolean Operators

|   Operator    |   Meaning   |
|---------------|-------------|
|   !           |   Negation of what follows |
|   &&          |     and     |
|   ||          |     or      |

It is important to note that `||` is an _inclusive_ or.  It evaluates to `true` if either operand is true.  This is different from many human languages, for which "or" is generally, though not always, _exclusive_.  An exclusive "or" is true only if exactly one of the conditions is true.
   You can have cake or ice cream (but not both).
An exclusive or can be constructed with 
```c++
(a && !b) || ( a && b)
```
where `a` and `b` are Boolean expressions.  An exclusive "or" operator is directly available only for bitwise comparisons.

## Conditional Operator Precedence

Like arithmetic operators, conditional operators have a precedence ordering.

* ! has the highest rank
* \>,>=,<,<= are equal and outrank == or !=
* ==,!= are equal and outrank &&
* && outranks ||

As always, use parentheses to change grouping or to improve clarity.

## If - Else If - Else

The `else if` and `else` are optional. The parentheses around the Boolean expression are required.
```c++
   if ( boolean1 ) {
      code for boolean1 true
   } else if ( boolean2 ) {
      more code for boolean2 true but boolean1 false
   } else {
      yet more code for both boolean1 and boolean2 false
   }
```
Here the curly braces denote a code block.  The `else if` and `else` can be on separate lines, but the layout illustrated is fairly conventional.

Only one branch will be executed.  Once any Boolean is determined to be true, the corresponding code will be executed and then the flow will proceed beyond the if block.

**Exercise**
Experiment with various truth values for bool1 and bool2.
{{< code-download file="/courses/cpp_introduction/codes/if_demo.cxx" lang="c++" >}}

## The ? Operator

C and C++ support a very succinct operator for cases in which the purpose of the if block is to assign a variable.
```no-highlight
expr1 ? expr2 : expr3
```
where `expr1` must evaluate to a Boolean.  The expressions `expr2` and `expr3` should evaluate to the same type.

The operation should be read as "IF expr1 THEN expr2 ELSE expr3".  It returns the value of the selected expression.
```c++
    float v = (y>=0.0) ? sqrt(y) : 0.0;
```
This is equivalent to
```c++
   float v;
   if (y>=0.0) {
       v=sqrt(y);
   } else {
       v=0.0;
   }
```

## Switch

Many "else ifs" can become confusing.  The `switch` construct can simplify the statements, under the right conditions.  
"Expression" must be an integer type or one that can be treated as an integer, e.g. `char` (but not string). 
The options must be literals or declared `const`.  The `break` statements are optional but cause the flow of control to jump out of the switch if one is executed.  Otherwise it will continue to the next case.
```c++
switch (expression) {
    case const value0:
        code;
        break;  //optional
    case const value1:
        code;
        break;  //optional
    case const value2:
        code;
        break;  //optional
    case const value3:
        code;
        break;
    default :   //optional
       code;
}
```
`default` is for the action, if any, to be taken if the expression does not evaluate to any of the options available.

Example:
{{< code file="/courses/cpp_introduction/codes/switch.cxx" lang="c++" >}}

Exercise:

This is the National Football Conference standings in late 2020:
   The Packers only need a win over the Chicago Bears to secure the No. 1 seed in the NFC. A loss or tie by the Seattle Seahawks would also give Green Bay the top spot.  If the Packers lose, though, the Seahawks or New Orleans Saints could claim the top spot. The Saints would secure the No. 1 seed with a Packers loss, a win and a Seattle win. Seattle can get it with a win, a Green Bay loss and a New Orleans loss or tie.

Write a program to determine whether the Packers will win the No. 1 seed.  Given the following conditions (not what happened), who won?

Packers lose to Bears
<br>
Seattle wins
<br>
The Saints tie 

Hint: code can often be simplified with the introduction of _logical variables_ which are sometimes also called _flags_.
<br>
Hint: if a variable is already a logical it is not necessary to test it against .true. or .false.
<br>
Hint: a loss or tie is not a win.

{{< spoiler text="Example Solution" >}}
{{< code-download file="/courses/cpp_introduction/solns/nfc.cxx" lang="c++" >}}
{{< /spoiler >}}
