---
title: Compound Types
toc: false
type: docs
draft: false
weight: 30
date: "2020-11-17T00:00:00"

menu:
    python-introduction:
---

The variables we have seen so far represent one single item each.  If `x` is a floating-point number, it takes on one value at a time.  Compound types are those for which a single variable represents many elements.  In Python, compound types can be sequences, which can be ordered by integer indexes.  We will focus on sequences here, since they are very widely used.  A few compound types are unordered, such as dictionaries and sets; we will study those later.

## Mutability

In Python types are categorized as __mutable__ or __immutable__.  Immutable types cannot be changed in place, but they can be overwritten.

```python
x=1.0
x=2.0
```

All the types we have seen so far are _immutable_.  Compound types may be _mutable_; their elements can be changed in place.  Compound types may also be immutable.  [Strings](/courses/python-introduction/strings) are a compound type but they are immutable.

```python
S1="Hello world"
S1[0:5]="Goodbye" #illegal
S1="Goodbye cruel world." #legal
```

