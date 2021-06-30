---
title: "Conditionals"
toc: true
type: book
weight: 31

menu:
    fortran_introduction:
        parent: Conditionals
        weight: 31

---

A _conditional_ is a programming construct that implements decisions. 
* _If_ the weather is good _then_ we will go for a walk, _else_ we will stay inside and watch TV.  
* _If_ it is cold enough to snow I will wear my heavy coat, _else if_ it is warmer and just rains I will wear my rainjacket.
The expression following each _if_ or _else_ must be true or false, i.e. a _logical_ expression (in Fortran terminology).

## Conditional Operators

Conditional operators are used to construct logical (Boolean) expressions.

### Numeric Operators

These are used to compare numerical values.
Fortran has two sets, one with letters and one with symbols.  Note that `/=` has a `/` for “not.”  The periods around the letter-based operators are required.

|   Letters    |   Symbols   |   Meaning  |
|--------------|-------------|------------|
|   .EQ.       |   ==        |   Equality |
|   .NE.       |   /=        | Not equal  |
|   .LT.       |    <        | Less than  |
|   .LE.       |    <=       | Less than or equal  |
|   .GT.       |    >        | Greater than  |
|   .GE.       |    >=       | Greater than or equal to  |

## Logical Operators

|   Operator    |   Meaning   |
|---------------|-------------|
|   .NOT.       |   Negation of what follows |
|   .AND.       |     and     |
|   .OR.        |     or      |

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

The `ELSEIF/ELSE IF` and `ELSE` are optional. The parentheses around the conditional are required.
```fortran
   IF ( comparison ) THEN
      code
   ELSEIF ( comparison) THEN
      more code
   ELSE
      yet more code
   ENDIF
```

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

SELECT Example:
```fortran
   select case (x)
      case  (:0)
         y=-x
      case (1)
         y=x+3.
      case (2:9)
         y=float(x)/2.
      case (10:20)
         y=float(x)/3.
      case default
         y=0.
   end select
```

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
{{< code-download file="/courses/fortran_introduction/solns/nfc.f90" lang="fortran" >}}
{{< /spoiler >}}

**Project**

Write a program to compute the day of the week for any date of the Gregorian calendar. Here is the formula: 
```
W=(C+Y+L+M+D ) mod 7 
```
Y is the last two digits of the actual year and D is the actual day. 
You need to obtain the value of C from the following rule for the years: 
* If year is in the 1400s, 1800s, 2200s, C=2 
* If year is in the 1500s, 1900s, 2300s, C=0
* If year is in the 1600s, 2000s, 2400s, C=5 
* If year is in the 1700s, 2100s, 2500s, C=4 
Months are numbered from 1 in the usual way, but (from January) M is 0, 3, 3, 6, 1, 4, 6, 2, 5, 0, 3, 5 

The only tricky part of this algorithm is L, the number of leap days that have occurred since the beginning of the century of the given date. 
To obtain this:
1. Integer divide the last two digits of the year by 4 to obtain the number of “ordinary” leap years in the century up to that year, not counting the century year itself if applicable. 
2. Obtain the remainder of the two digits and 4. If it is not a century year and the remainder is 0 the year is a leap year, otherwise it is not. If the year itself is a century year see Step 3. 
3. If the century (1400, 1500, etc.) was evenly divisible by 400 then the century year is a leap year, otherwise it is not. Thus 2000 was a leap year but 1900 was not. So add 1 for centuries divisible by 400 and 0 otherwise. 
4. If your date is January 1-February 29 of a leap year, subtract 1. 
Try to devise a method to obtain the last two digits on your own. Print the day of the week as a word (Monday, Tuesday, etc.). Remember that Sunday is the first day of the week and it will be counted as 0 in this algorithm. 

Test your program first with your own birth date. Then test with the following dates: 
* Today’s date 
* December 25, 1642 (Note: this is Newton’s birth date in the Julian calendar, but use it as a Gregorian date) 
* October 12, 1492 
* January 20, 2000 
* December 11, 2525

Try to write and test your own program before peeking at the sample solution.

{{< spoiler text="Sample solution" >}}
{{< code file="/courses/fortran_introduction/solns/day_of_week.f90" lang="fortran" >}}
{{< /spoiler >}}
