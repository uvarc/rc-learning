---
title: Conditionals and Loops
toc: true
type: docs
draft: false
sections_weight: 5
menu:
  python_introduction:
    parent: Introduction to Programming in Python
    weight: 5
---

Now that we understand the basic types, we can begin to study the fundamental programming constructs that all programs will use.  The two most important of these are _conditionals_ and _loops_.

## Conditionals

Computer programs really can't do all that many things.  They can

* assign values to variables (memory locations)
* make decisions based on comparisons
* repeat a sequence of instructions over and over
* call subprograms

Decisions (conditionals) are one of these basic constructs.

### Branching

When a program makes a decision, it is said to _branch_.  A program may be regarded as a tree with one root and many branches.  Executing the program causes the branches to be followed in a particular order.

##### Python if-else if-else

Most languages branch with some variant of 

if (something that evalutes to True or False) do something
else if (comparison) do some other things
else do some default things

Only the `if` is required; the other statements are optional.  The specific syntax in Python is

```python
if some_condition:
    code block
elif another_condition:
    code block
else:
    code block
```

Observe the colons and the indentation.  To terminate the conditional, return to the same indentation level as the initial `if`.

If the "code block" consists of a single statement, it is permissible to write it on the same line as the if or elif

```python
if x==0: z=0
```

Conditionals may be nested 

```python
if some_condition:
    if another_condition:
        do_something
    else:
        do_something_else
elif condition2:
    if something:
        do_work
```

Some languages have a "case" or "switch" statement for a long sequence of options.  Python has no such construct so we use a series of `elif`s.

```python
if cond1:
    block1
elif cond2:
    block2
elif cond3:
    block3
elif cond4:
    block4
else:
    final choice
```

The "condition" must be an expression that evaluates to True or False; that is, it must be a Boolean variable or expression.  Boolean expressions are formed from other types of variables with conditional operators.

<details>
<summary>Exercise 6</summary>

The Body Mass Index is a widely-used number to classify body shapes.  The formula in Imperial units (pounds, inches) is

BMI=weight\*703.1/height\*\*2

In metric units (kg, m) the formula is

BMI=weight/height\*\*2

The categories are as follows (we have omitted the top two):

Under 18.5: underweight
18.5 to 25: normal 
over 25 to 30: overweight
over 30 to 35: obese class I
over 35 to 40: obese class II
over 40 to 45: obese class III
over 45: obese class IV (morbidly obese)

Using whichever unit system you prefer, write some code to assign the weight and height, compute the number, and determine its classification.  Assign your own weight and height.  Try a few others.  Use an online calculator to check your results.

</details>

## Loops

One of the most fundamental processes in a computer program is to repeat statements many (perhaps many, many, many) times.  Computers never run out of patience.

Like most languages, Python has two major types of loops.  

_For loops_ execute for a fixed number of iterations.  It is possible to exit early, but this must be explicitly added to the code. 

_While loops_ do not start with a predetermined number of iterations.  They terminate when some condition becomes False.

### For Loops in iPython

```python
for item in iterator:
    block1
else:
    block2
```

The `else` clause is executed if the loop completes all iterations and is optional.  The colons are required as indicated.  Code blocks must be indented.  The `item` is a variable which successively takes on the values in the `iterator`.  An iterator is a data structure through which we can step, item by item.  Until we study some more general examples, let's look at the __range__ iterator. 

##### Range

The range iterator is used to step over a sequence of numbers.  It takes up to three arguments.

* range(10)  : 0,1,2,3,4,5,6,7,8,9
* range(1,10) : 1,2,3,4,5,6,7,8,9
* range(0,10,2) : 0,2,4,6,8
* range(10,0,-2) : 10,8,6,4,2 (note that zero is __not__ included)

The interval is often called a stride.  If it is present the lower bound must also be present even if it is the default, 0.  Otherwise the lower bound may be omitted.  If the stride is omitted it is 1.  The last value is never reached.

<details>
<summary>Exercise 7</summary>

Execute the following for loop:

```python
for i in range(10):
    print(i)
```

Modify this loop to print the values of i for 

```python
range(10)
range(1,10)
range(0,10,2)
range(1,0,-2)
```

Modify your loop to print the first N integers.  Be sure that N is set to a value before you try to run the loop.
Write a loop that will sum the first N integers.  Hint: you will need a variable called an <em>accumulator</em> whose value starts outside the loop at 0.

</details>

##### Enumerate

Sometimes we need both the item and its index.  We can use enumerate for this purpose.

```python
velocity=[-11.,-3.,-1.,1.,2.3,.4.]
for i,v in enumerate(velocity):
    print(i,v)
```

## While loops

A _while_ loop uses a conditional to determine when to exit.  The loop must be coded to ensure that the conditional will become False at some point, or it will never terminate.

Python syntax

```python
while conditional:
    block1
else:  #optional
    block2
```

As for the _for_ loop, colons and indentations are required.  The optional _else_ clause is executed if and only if the conditional becomes False, which for a while loop is normal termination.

```python
x=-20
y=-10
while x<0 and y<0:
    x=10-y
    y=y+1
    z=0
print(x,y,z)
```

<details>
<summary>Exercise 8</summary>
<pre>
Modify each for loop in the previous exercise to use a while loop instead.
</pre>
</details>

## Leaving Early

To exit a loop prior to its normal termination, use the `break` statement.  

If `break` is executed, any `else` clause will _not_ be executed.

Break can exit only from the loop level in which it occurs.  In Python this is the indentation level.

To skip the rest of the loop instrutions and go to the next cycle, use `continue`. Similarly to `break`, it skips only the rest of the statements at its own level.

```python
x=1.
while x>0:
    x+=1.
    if x>=1000.0: break 
    if x<100.0: continue
    x+=20.
    print(x)
```

Now we can better understand the purpose of the `else` clause.  It can provide a warning for cases where a loop terminating normally may indicate a failure.  For example, if we were iterating on an algorithm that converges to an answer but may go astray, we can set a limit on the maximum number of iterations. In this example, optimize is a function that we invoke.  It may be in another package or it may be something we wrote.  The variable f stands for the function we are optimizing.  Therefore this code fragment is incomplete and cannot be run as is; it is an example of how `else` works.

```python
max_iter=1000000000
tol=1.e-14
iter=0
while iter<max_iter:
    x_new=optimize(f,x_old)
    iter+-1
    if abs(x_new-x_old)<tol:
        break
    x_old=x_new
else:
    print("Warning: max_iter exceeded, solution may not be valid.")
```

## Repeat/Until

A while loop is equivalent to

```python
while True:
    if not condition:
        break 
    codeblock
```

The while always tests at the _top_ of the loop.  If we wish to test elsewhere we can use break to accomplish this.  If we test at the bottom of the loop, the pattern is often called _repeat/until_.

```python
while True:
   codeblock
   if condition: break 
```

Example:

```python
x=1.
while True:
    z=x-1
    x+=1
    if x>10: break 
```

### Nested Loops

We can write loops within loops.  The outer loop variable remains fixed while the inner one goes through its iterator; then the outer one takes the next value and the entire inner loop is repeated.

```python
for i in range(5):
    for j in range(10):
        print(i,j)
```

#### Reinitializing

In nested loops, if we need to recompute something we often need to reinitialize a variable.  Examine the difference between

```python
s=0.
for i in range(10):
    for j in range(15):
        s=s+i+j
print(s)
```

and 

```python
for i in range(10):
    s=0 
    for j in range(10):
        s=s+i+j
print(s)
```
