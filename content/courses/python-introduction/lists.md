---
title: Lists
toc: true
type: docs
draft: false
weight: 32

menu:
    python-introduction:
        parent: Compound Types
---

Lists are one of the most important data types in Python.  They are flexible and easy to use. Lists are [sequences](/courses/python-introduction/sequences) of objects.  Each element of the list can be of any type, including another list.  
Lists have all the common properties of sequences: they are _ordered_ which means that each element can be referenced by an integer _index_.  As for all ordered types in Python, the indices start at 0.  

Lists are dynamically sized and they are __mutable__.  They must be declared in some manner before they can be used.

* Empty list
  * `L=[]`
* List with specified elements (the values of the elements must have been assigned)
  * `L=[L1,L2,L3,L4,L5]
* List with `N` items, each having value `V` (both N and V must have been assigned and N must be an integer)
  *  `L=[V]*N`
* Return a new list from a built-in function
  * `L=list(range(12))`
    This returns a list of integers 0,1,..11

#### List Elements

To access a particular element by its index, we enclose the index value in square brackets.  If a list `L` has been defined, we can access its third element with
```python
print(L[2])
```

Lists are mutable so individual elements can be changed.
```python
L[2]=42
```

**Exercise**
In your choice of JupyterLab or Spyder, type and run
```python
myL=[1,2,3,5,6,7]
myL[1]=4
print(myL)
```

Sublists are often called _slices_.

```python
subL=myL[1:3]
```
Always remember that the upper bound is **excluded**, so this slice is elements 1 and 2, which are the second and third elements.

```python
subL=myL[2:]
```
This extracts elements from the third to the last.

```python
subL=myL[:3]
```
This extracts the elements from the beginning to the third element (index number 2).

An increment can also be specified

```python
subL=myL[1:7:2]
```
This extracts elements 1, 3, and 5.

#### Changing Lists

* Initialize an empty list
  * `L=[]`
* Append an element to a list
  * `L1.append("Graham")`
* Extend a list with another list
  * `L1.extend(["Michael","Terry"])`

It is important to understand the difference btween appending and extending. Appending adds the argument as the new last element exactly as it appears. It takes any type.  Extending requires a list as its argument.  It joins the two lists sequentially.  It is is equivalent to
  * `L=[1,2,3]+[4,5,6]`

Joining two compound types in a series is called _concatenation_.  We frequently concatenate lists and strings in Python.

* Insert an element
  * `L.insert(i,item)`
  * This inserts `item` before element `i`. To add an item at the beginning of the list, use
    * `L.insert(0,item)`

Shortening lists:

* Delete an element by its index
  * `del L[i]`
* Delete the first occurrence of a particular element
  * `L.remove(item)`
  * The `item` must match exactly or an error occurs.
* Remove and return an element
  * `item=L.pop(<i>)`
  * The angle brackets indicate an optional argument and are not typed.  If the argument is absent the _last_ element is returned.  If it is present that value is returned.
    * `lastVal=L.pop()`
    * `A_val=L.pop(2)`
  * Keep in mind that `pop` shortens the list.

Much more can be done with lists.

* Length (number of elements)
  * `lenL=len(L)`
* Sum of values (if all elements are the same type and sum is defined for them)
  * `sumL=sum(L)`
* Maximum or minimum value of the items (if they are the same type and max and min are defined)
  * `max(L) min(L)`
* Membership test (returns Boolean)
  * `item in list`
* Index of first time item occurs
  * `myIndex=L.index(item)`
* Number of times item occurs
  * `numItem=L.count(item)`
* Sort a list (when possible) in place (overwrites the original)
  * `L.sort()`
* Return a sorted list to a new list
  * `Lsorted=sorted(L)`
* Reverse the list in place (overwrites)
  * `L.reverse()`
* There is no direct function to reverse and return into another list, so we use this handy trick
  * `Lreversed=L[::-1]`
* In Python 3, `reversed(L)` returns an iterator and not a list, but you may use the list constructor to convert it.
  * `Lreversed=list(reversed(L))`

**Exercise**

Type

```python
numList=list(range(10))
```

Print the length of the list.
Change the fourth element to 11.
Extend the list with L=[20,30,40].
Print the index of the item that has the value `9`.
Remove that item from the list.
Print the current length of the list.
Sort the list and then reverse the sorted version.

Copying lists:

```python
A=[1,2,3,4]
B=A
print(A)
B[2]=9
print(A)
```

`B` is just an alias (a "nickname") for `A`.  If `B` changes so does `A`.  __This is true for all mutable types.__ Slicing notation creates a _view_ that can make a copy if the entire list is included.

```python
C=A[:]
C[1]=11
print(A)
print(C)
```

An alternative is to explicitly use the list constructor function:

```python
D=list(A)
```
