---
title: Sets
toc: true
type: docs
draft: false
weight: 72

menu: 
    python-introduction:
        parent: Dictionaries and Sets
---

Sets are another _unordered_ type.  No element of a set may be duplicated.  The set is mutable but all elements must be immutable.

Create the set with the set() function.  Empty parentheses will not work because those are used for an empty tuple (which will thus remain empty).

A frequent application of sets is to eliminate duplicates.

```python
L=[0,0,1,4,8,8,10]
M=list(set(L))
print(M)
```

Since sets are unordered, the order of `L` is not guaranteed to be preserved.

### Set Operations

* Add an element to set s
  * `s.add(item)`
* Extend with a sequence 
  * `s.update(t)`
* Remove an item (will fail silently if the item isn't present)
  * `s.discard(item)`
* Remove with an exception if the item isn't is_present 
  * `s.remove(item)`

The `in` operator works even though, strictly speaking, sets are not sequences.

```python
item in s
```

Sets attempt to reproduce most of the properties of mathematical sets.  

* Test for subset (a set is a subset of itself):
  * `s2.issubset(s1)` or `s2<=s1`
* Test for superset (similarly, a set is its own superset):
  * `s1.issuperset(s2)` or `s2>=s1`
* Intersection:
  * `s1.intersection(s2)` or `s1&s2`
* Union -- the | symbol is a pipe:
  * `s1.union(s2)` or `s1|s2`
* Symmetric difference (elements in one or the other but not both):
  * `s1.symmetric_difference(s2)` or `s1^s2`
* Set difference (elements in s1 but not in s2):
  * `s1.difference(s2)` or `s1-s2`

**Exercise**

Type at the interpreter 

```python
s=set()
s.update("California")
print(s)
```

What happened?  Lesson: be careful with strings since they are sequences.

```python
states={"Alabama","Arkansas","California","California"}
```

Initialization with curly braces has been valid since Python 2.6.  Since there are no key-value pairs it will not be construed as a dictionary.

```python
print(states)
states2=set()
states2.add("California")
states2.add("Colorado")
states2.add("Oregon")
states-states2
states&states2
states^states2
states|states2
```
