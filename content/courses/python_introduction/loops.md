---
title: Loops
toc: true
type: book
draft: false
weight: 43
---

One of the most fundamental processes in a computer program is to repeat statements many (perhaps many, many, many) times.  Computers never run out of patience.

Like most languages, Python has two major types of loops.  

_For loops_ execute for a fixed number of iterations.  It is possible to exit early, but this must be explicitly added to the code. 

_While loops_ do not start with a predetermined number of iterations.  They terminate when some condition becomes False.

## For Loops in iPython

```python
for item in iterator:
    block1
else:
    block2
```

The `else` clause is executed if the loop completes all iterations and is optional.  The colons are required as indicated.  Code blocks must be indented.  The `item` is a variable which successively takes on the values in the `iterator`.  An iterator is a data structure through which we can step, item by item.  Until we study some more general examples, let's look at the __range__ iterator. 

### Range

The range iterator is used to step over a sequence of numbers.  It takes up to three arguments.

* `range(10)`  : 0,1,2,3,4,5,6,7,8,9
* `range(1,10)` : 1,2,3,4,5,6,7,8,9
* `range(0,10,2)` : 0,2,4,6,8
* `range(10,0,-2)` : 10,8,6,4,2 (note that zero is __not__ included)

The interval is often called a stride.  If it is present the lower bound must also be present even if it is the default, 0.  Otherwise the lower bound may be omitted.  If the stride is omitted it is 1.  The last value is never reached.

In Python 3 the range function returns an iterator object and is not directly accessible.  To see the values, convert it to a list
```python
print(list(range(10)))
```

<details>
<summary>Exercise 7</summary>

Execute the following for loop:

```python
for i in range(10):
    print(i)
```

Modify this loop to print the values of `i` for 

```python
range(10)
range(1,10)
range(0,10,2)
range(1,0,-2)
```

Modify your loop to print the first N integers.  Be sure that N is set to a value before you try to run the loop.

Write a loop that will sum the first N integers.  Hint: you will need a variable called an <em>accumulator</em> whose value is assigned outside the loop to 0.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/sum_to_N.py" lang="python" >}}
{{< /spoiler >}}

</details>

### Enumerate

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

{{< code-snippet >}}
x=-20
y=-10
while x<0 and y<0:
    x=10-y
    y=y+1
    z=0
print(x,y,z)
{{< /code-snippet >}}

<details>
<summary>Exercise 8</summary>
<pre>
Modify each for loop in the previous exercise to use a while loop instead.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/sum_to_N_while.py" lang="python" >}}
{{< /spoiler >}}

</pre>
</details>

## Leaving Early

To exit a loop prior to its normal termination, use the `break` statement.  

If `break` is executed, any `else` clause will _not_ be executed.

Break can exit only from the loop level in which it occurs.  In Python this is the indentation level.

To skip the rest of the loop instructions and go to the next cycle, use `continue`. Similarly to `break`, it skips only the rest of the statements at its own level.

```python
x=1.
while x>0:
    x+=1.
    if x>=1000.0: break 
    if x<100.0: continue
    x+=20.
    print(x)
```

Now we can better understand the purpose of the `else` clause.  One application could be to provide a warning for cases where a loop terminating normally may indicate a failure.  For example, if we were iterating on an algorithm that converges to an answer but may go astray, we can set a limit on the maximum number of iterations. In this example, optimize is a function that we invoke.  It may be in another package or it may be something we wrote.  The variable `f` stands for the function we are optimizing.  Therefore this code fragment is incomplete and cannot be run as is; it is an example of how `else` works.

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

{{< code-snippet >}}
s=0.
for i in range(10):
    for j in range(15):
        s=s+i+j
print(s)
{{< /code-snippet >}}

and 

{{< code-snippet >}}
for i in range(10):
    s=0 
    for j in range(10):
        s=s+i+j
print(s)
{{< /code-snippet >}}


## List Comprehensions

A list comprehension collapses a loop over a list and, optionally, an if clause.

```python
squares=[x**2 for x in range(10)]
```

This is equivalent to

```python
for x in range(10):
    squares.append(x**2)
```

With an optional conditional it becomes

```python
positives=[math.sqrt(x) for x in range(-10,11) if x>0]
```

This is equivalent to

```python
for x in range(-10,11):
    if x>0:
        positives.append(math.sqrt(x))
```

List comprehensions are nearly always __much__ faster than the equivalent loop.
It is good practice to replace a short, simple loop with a comprehension whenever possible.
