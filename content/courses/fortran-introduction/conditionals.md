---
date: "2021-04-05"
title: "Conditionals"
weight: 31
---

A _conditional_ is a programming construct that implements decisions. 
* _If_ the weather is good _then_ we will go for a walk, _else_ we will stay inside and watch TV.  
* _If_ it is cold enough to snow I will wear my heavy coat, _else if_ it is warmer and just rains I will wear my rain jacket.
The expression following each _if_ or _else_ must be true or false, i.e. a _logical_ expression (in Fortran terminology).

## Conditional Operators

Conditional operators are used to construct logical (Boolean) expressions.

### Numeric Operators

These are used to compare numerical values.
Fortran has two sets, one with letters and one with symbols.  Note that `/=` has a `/` for “not.”  The periods around the letter-based operators are required.

| Letters | Symbols | Meaning                  |
|---------|---------|--------------------------|
| .EQ.    | ==      | Equality                 |
| .NE.    | /=      | Not equal                |
| .LT.    | <       | Less than                |
| .LE.    | <=      | Less than or equal       |
| .GT.    | >       | Greater than             |
| .GE.    | >=      | Greater than or equal to |

## Logical Operators

| Operator | Meaning                  |
|----------|--------------------------|
| .NOT.    | Negation of what follows |
| .AND.    | and                      |
| .OR.     | or                       |

It is important to note that `.OR.` is an _inclusive_ or.  It evaluates to `.TRUE.` if either operand is true.  This is different from many human languages, for which "or" is generally, though not always, _exclusive_.  An exclusive "or" is true only if exactly one of the conditions is true.
   You can have cake or ice cream (but not both).
An exclusive or can be constructed with 
```fortran
(a .AND. .NOT. b) .OR. (.NOT. a .AND. b)
```
where `a` and `b` are logical expressions.

## Conditional Operator Precedence

Like arithmetic operators, conditional operators have a precedence ordering.

* .NOT. has the highest rank
* \>,>=,<,<= are equal and outrank == or /=
* ==,/= are equal and outrank .AND.
* .AND. outranks .OR.

As always, use parentheses to change grouping or to improve clarity.

## IF-THEN-ELSE

The `ELSEIF/ELSE IF` and `ELSE` are optional. The parentheses around the logical expression are required.
```fortran
   IF ( logical ) THEN
      code
   ELSEIF ( logical) THEN
      more code
   ELSE
      yet more code
   ENDIF
```
Only one branch will be executed.  Once any logical expression is determined to be true, 
the corresponding code will be executed and then the flow will proceed beyond the if block.

**Exercise**
Experiment with various truth values for bool1 and bool2.
{{< code-download file="/courses/fortran-introduction/codes/if_demo.f90" lang="fortran" >}}

## SELECT CASE

Many "else ifs" can become confusing.  The `SELECT CASE` construct can simplify the statements, under the right conditions.  "Expression" refers to any valid
expression that can be evaluated to "value0", "value1", etc.
```fortran
   SELECT CASE (expression)
      CASE(:value0)   ! Expression <= value0
         code
      CASE(value1)
        code
      CASE(value2)
        code
      CASE(value3:)  ! Expression >=value3
        code
      CASE (value4,value5,value6) !Multiples OK
        code
      CASE (value7:value9)
        code
      CASE DEFAULT    ! Optional
        code
   END SELECT
```
"Expression" must be character, integer, or logical.
Ranges are only applicable for numeric or character expressions.
`DEFAULT` is for the action, if any, to be taken if the expression does not evaluate to any of the options available.

**Example**
{{< code-download file="/courses/fortran-introduction/codes/selectcase.f90" lang="fortran" >}}

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
{{< code-download file="/courses/fortran-introduction/solns/nfc.f90" lang="fortran" >}}
{{< /spoiler >}}
