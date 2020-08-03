---
# date: 2019-09-114T10:00:00-00:00
title: Lists and Tuples
toc: true
type: docs
date: "2019-05-05T00:00:00+01:00"
draft: false
menu:
  python_introduction:
    parent: Introduction to Programming in Python
    weight: 4
---

## Compound Types

So far we have examined _primitive_ types.  Each variable stands for one value.  Python defines several built-in _compound types_; a single variable stands for multiple values. 

### Sequences

A _sequence_ in Python is an ordered group of values that can be represented by a single value. We can address individual elements or subgroups of a sequence with square brackets and possibly a range.  Square brackets hold the index or range.  

```
A[i]
A[1:j+k+1]
```

Python is _zero based_ (the first element is numbered 0) and the upper bound of _any_ range is always _non_inclusive_.  Python defines several intrinsic sequences: strings, unicode strings, lists, tuples, and a few others that we will not cover.

Sequences either are directly iterators or can be readily converted to an iterator.

#### Sequence Operators

* Belonging
  * in operator.  `x in S` returns True or False if x is or is not an element of the sequence S
* Identity
  * is operator.  `S1 is S`2 returns True or False if S1 and S2 are exactly the same or different.
  * is can be negated `S1 is not S2`
* Range extraction
  * S\[il:ul+1] starts at il and goes to ul.  The colon is here called a range operator.
* Starting from the end 
  * S\[-N] is the N-1 element.  Thus S\[-1] is the last element, S\[-2] the next to last, and so forth. 
* Concatenation
  * S1+E1
* Repetition
  * S1\*N replicates the sequence S1 N times.  

<details>
<summary>Exercise 7</summary>
Examine the results of the following:
<pre>
<code>
A=[1.,2,3.,4.,5,6]
1 in A
1. in A
9 in A
9 not in A
</code>
Remember that 1 and 1. are _different_ types. 
<code>
A[3]
B=A[:]
C=A
B is A
C is A
B is not A
</code>
</pre>
</details>

### Mutability

In Python types are categorized as __mutable__ or __immutable__.  Immutable types cannot be changed in place, but they can be overwritten.

```
x=1.0
x=2.0
```

All the types we have seen so far are _immutable_.  Compound types may be _mutable_; their elements can be changed in place.  Compound types may also be immutable.  Strings are a compound type but they are immutable.

```
S1="Hello world"
S1[0:5]="Goodbye" #illegal
S1="Goodbye cruel world." #legal
```

## Lists

Lists are one of the most important data types in Python.  They are flexible and easy to use. Lists are ordered collections of objects.  Each element of the list can be of any type, including another list.  Lists are _ordered_ which means that each element can be referenced by an integer _index_.

Lists are dynamically sized and they are __mutable__.  Unlike most variables in Python, they must be declared in some manner before they can be used.

* Empty List
  * L=\[]
* Return a new list from a built-in function
  * L=list(range(12))
    This returns a list of integers 0,1,..11

As for all ordered types in Python, the indices start at 0.  The upper bound in any range is the limit we _do not_ reach.

#### List Elements

To access a particular element by its index, we enclose the index value in square brackets.

```
L[2]
```

Lists are mutable so individual elements can be changed.

```
myL=[1,2,3]
myL[1]=4
print(myL)
```

Sublists are obtained must like substrings.  They are often called _slices_.

```
subL=L[1:3]
```

Here the colon is again the range operator.  Always remember that the upper bound is excluded, so this slice is elements 1 and 2, which are the second and third elements.

```
subL=L[2:]
```

This extracts elements from the third to the last.

```
subL=L[:3]
```

This extracts the elements from the beginning to the third element (index number 2).

A stride can also be specified

```
subL=L[1:7:2]
```

This extracts elements 1, 3, and 5.

#### Changing Lists

* Initialize
  * L=\[]
* Initialize a list of known size (the value can be a variable but must have a value when this statement is executed)
  * L1=[0]\*N
* Append an element to a list
  * L1.append("Graham")
* Extend a list with another list
  * L1.extend(["Michael","Terry"])

Appending adds the argument as the new last element exactly as it appears. It takes any type.  Extending requires a list as its argument.  It concatenates that list at the end of the original one.  It is is equivalent to

* L=[1,2,3]+[4,5,6]
* insert an element
  * L.insert(i,item)
  * This inserts `item` before element `i`. To add an item at the beginning of the list, use
    * L.insert(0,item)

Shortening lists:

* Delete an element by its indext
  * del L[i]
* Delete the first occurrence of a particular element
  * L.remove(item)
  * The `item` must match exactly or an error occurs.
* Remove and return an element
  * item=L.pop(\\&lt;i>)
  * The angle brackets indicate an optional argument.  If it is absent the _last_ element is returned.  If it is present that value is returned.
    * lastVal=L.pop()
    * A_val=L.pop(2)
  * Keep in mind that pop shortens the list.

Much more can be done with lists.

* Length
  * lenL=len(L)
* Maximum or minimum value of the items (if they are the same type)
  * max(L) min(L)
* Membership test (returns Boolean)
  * item in list
* Index of first time item occurs
  * myIndex=L.index(item)
* Number of times item occurs
  * numItem=L.count(item)
* Sort a list (when possible) in place (overwrites the original)
  * L.sort()
* Return a sorted list to a new list
  * Lsorted=sorted(L)
* Reverse the list in place (overwrites)
  * L.reverse()
* There is no direct function to reverse and return another list, so we use this handy trick
  * Lreversed=L\[::-1]
* In Python 3, `reversed(L)` returns an _iterator_ and not a list, but you may use 
  * Lreversed=list(reversed(L))

<details>
<summary>Exercise 8</summary>
<pre>
<p>
Type
<code>
numList=list(range(10))
</code>
Print the length of the list.
Change the fourth element to 11.
Extend the list with L=[20,30,40]
Print the index of the item 9
Remove that item from the list.
Printe the current length of the list.
Sort the list and then reverse the sorted version.
</p>
</pre>
</p>
</details>

Copying lists:

```
A=[1,2,3,4]
B=A
print(A)
B[2]=9
print(a)
```

B is just an alias (a "nickname") for A.  If B changes so does A.  __This is true for all mutable types.__ Slicing notation creates a _view_ that can make a copy if the entire list is included.

```
C=A[:]
C[1]=11
print(A)
print(C)
```

An alternative is to explicitly use the list constructor function:

```
D=list(A)
```

## Tuples

A tuple is a Python ordered sequence object that is similar to a list but is __immutable__.  Tuples are indicated by parentheses (round brackets). Like all ordered sequences, we can refer to individual elements with [n] and slices with [lb:ub].  As for all other ordered sequences, numbering of elements starts at 0 and the upper bound of a range is excluded.

Creation
  T=tuple((1,2,3))
Length
  len(T)
Concatenation (as for strings, must assign to a new variable)
  T3=T+T2
Membership
  3 in T
Iteration
  for i in T: print(i)

Although the tuple is immutable, any mutable _elements_ can be changed.

```
myList=list()
t=(myList,myList)
myList.append(1)
print(t)
myList.append(2)
print(t)
```

Since they are immutable, tuples have fewer defined operations than lists.  They can be indexed and slices like lists.

```
T=(1,2,3)
T2=T[1:]
```

One important set of operations on tuples is packing and unpacking.  If the context is unambiguous, the parentheses are not required.

```
T=1,2,3
print(type(T))
x,y,z=T
print(x,y,z)
```

Occasionally we need a tuple with one element.  This is not the same thing as a single variable so we must distinguish them.  A tuple with one element _must_ be declared like (E,) -- the comma is required.

#### Lists or Tuples?

A tuple should be used whenever the structure should not be dynamically changed or resized.  

Tuples are preferred over lists for returning multiple values from functions.  

Tuples are often used for heterogenous data, i.e. elements of different types.  List elements are typically homogeneous (all the same type) though this is not a requirement.

