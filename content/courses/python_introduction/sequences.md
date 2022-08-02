---
title: Sequences
toc: true
type: docs
draft: false
weight: 30

menu:
    python_introduction:
        parent: Compound Types
        weight: 30
---

A _sequence_ in Python is an ordered group of values that can be represented by a single variable. We can address individual elements or subgroups of a sequence with square brackets and possibly a _range_ of indices.  Square brackets hold the index or range. 

```python
A[0]
A[2:5]
A[i]
A[1:j+k+1]
```

Python is _zero based_ (the first element is numbered 0) and the upper bound of _any_ range is always *non*inclusive.  Python defines several intrinsic sequences: strings, Unicode strings, lists, tuples, and a few others that we will not cover.

An _iterator_ is a data type that can be traversed in order.  Sequences either are directly iterators or can be readily converted to iterators.

## Sequence Operators

* Belonging
  * `in` operator.  `x in S` returns True or False if x is or is not an element of the sequence S.
* Identity
  * `is` operator.  `S1 is S2` returns True or False if S1 and S2 are exactly the same or different.  "Exactly the same" is quite rigid in Python so check documentation for the behavior of `is` with different objects.
  * `in` and `is` can be negated. `S1 is not S2`; `A not in B`.
* Range extraction
  * `S[il:ul+1]` starts at `il` and goes to `ul`.  The colon is here called a range operator.
* Starting from the end 
  * `S[-N]` is the `N-1` element.  Thus `S[-1]` is the last element, `S[-2]` the next to last, and so forth. 
* Concatenation
  * `S1+E1`
* Repetition
  * `S1*N` replicates the sequence `S1` `N` times.  

**Exercise**

Examine the results of the following:

```python
A=[1.,2,3.,4.,5,6]
1 in A
1. in A
9 in A
9 not in A
```
Remember that 1 and 1. are _different_ types. 
```python
A[3]
B=A[:]
C=A
B is A
C is A
B is not A
```
