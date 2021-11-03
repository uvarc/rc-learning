---
title: List Comprehensions
toc: true
type: book
draft: false
weight: 45
---

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

