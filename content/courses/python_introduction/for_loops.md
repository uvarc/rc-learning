---
title: For Loops
toc: true
type: docs
draft: false
weight: 42

menu:
    python_introduction:
        parent: Conditionals and Loops
        weight: 42
---

One of the most fundamental processes in a computer program is to repeat statements many (perhaps many, many, many) times.  Computers never run out of patience.

Like most languages, Python has two major types of loops.  

_For loops_ execute for a fixed number of iterations.  It is possible to exit early, but this must be explicitly added to the code. 

_While loops_ do not start with a predetermined number of iterations.  They terminate when some condition becomes False.

## For Loops in Python

```python
for item in iterator:
    block1
else:
    block2
```

The `else` clause is executed if the loop completes all the steps and is optional.  The colons are required as indicated.  Code blocks must be indented.  The `item` is a variable which successively takes on the values in the `iterator`.  

### Iterables and Iterators

An _iterable_ is any data structure that can step through items one at a time.  An _iterator_ is the structure by which an iterable is traversed. Some data structures, such as `range`, are direct iterators, whereas others, such as strings, are iterables with the ability to be iterated in a for loop.

#### Range

The range iterator is used to step over a sequence of numbers and is very frequently used with for loops.  It takes up to three arguments.

* `range(10)`  : 0,1,2,3,4,5,6,7,8,9
* `range(1,10)` : 1,2,3,4,5,6,7,8,9
* `range(0,10,2)` : 0,2,4,6,8
* `range(10,0,-2)` : 10,8,6,4,2 (note that zero is __not__ included)

The interval is often called a stride.  If it is present the lower bound must also be present even if it is the default, 0.  Otherwise the lower bound may be omitted.  If the stride is omitted it is 1.  The last value is never reached.

In Python 3 the range function returns an iterator object and is not directly accessible.  To see the values, convert it to a list
```python
print(list(range(10)))
```

**Exercise**

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
{{< code-download file="/courses/python_introduction/exercises/sum_to_N.py" lang="python" >}}
{{< /spoiler >}}

#### Other Iterables

Any ordered sequence can be iterated.  Common types used with for loops are lists and strings.

```python
cast=['John','Michael','Terry','Graham','Eric']
for player in cast:
    print("One of the Pythons:",player)
```

Another example:

```python
for ch in "HelloEverybody":
    print(ch)
```

### Enumerate

Sometimes we need both the item and its index.  We can use enumerate for this purpose.

```python
velocity=[-11.,-3.,-1.,1.,2.3,.4.]
for i,v in enumerate(velocity):
    print(i,v)
```

