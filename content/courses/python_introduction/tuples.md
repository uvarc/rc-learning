---
title: Tuples
toc: true
type: book
draft: false
weight: 33
---

A tuple is a Python ordered sequence object that is similar to a list but is __immutable__.  Tuples are indicated by parentheses (round brackets). Like all ordered sequences, we can refer to individual elements with [n] and slices with [lb:ub].  As for all other ordered sequences, numbering of elements starts at 0 and the upper bound of a range is excluded.

* Creation
  * `T=tuple((1,2,3))`
* Length
  * `len(T)`
* Concatenation (as for strings, must assign to a new variable)
  * `T3=T+T2`
* Membership
  * `3 in T`
* Iteration
  * `for i in T: print(i)`

Although the tuple is immutable, any mutable _elements_ can be changed.

```python
myList=list()
t=(myList,myList)
myList.append(1)
print(t)
myList.append(2)
print(t)
```

Since they are immutable, tuples have fewer defined operations than lists.  They can be indexed and sliced like lists.

```python
T=(1,2,3)
T2=T[1:]
```

One important set of operations on tuples is packing and unpacking.  If the context is unambiguous, the parentheses are not required.

```python
T=1,2,3
print(type(T))
x,y,z=T
print(x,y,z)
```

Occasionally we need a tuple with one element.  This is not the same thing as a single variable so we must distinguish them.  A tuple with one element _must_ be declared like (E,) -- the comma is required.

## Lists or Tuples?

A tuple should be used whenever the structure should not be dynamically changed or resized. 

Tuples are preferred over lists for returning multiple values from functions. 

Tuples are often used for heterogeneous data, i.e. elements of different types.  List elements are typically homogeneous (all the same type) though this is not a requirement.

## Resources

Lists, tuples, and the range iterator are described [here](https://docs.python.org/3/library/stdtypes.html#sequence-types-list-tuple-range).

