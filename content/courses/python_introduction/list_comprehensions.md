---
title: Loop Alternatives
toc: true
type: book
draft: false
weight: 45
---

Loops in Python are very slow.  Nested loops are especially slow.  Some alternatives are available in the standard set of packages that are usually faster..

## List Comprehensions

A list comprehension collapses a loop over a list and, optionally, an if clause.

```python
squares=[x**2 for x in range(10)]
```

This is equivalent to

```python
squares=[]
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

List comprehensions may be nested.

{{< code-snippet >}}
list_2d = [[i+j for j in range(1,6)] for i in range(10,16)]
{{< /code-snippet >}}

Observe the ordering in the previous example.  The inner loop is _first_.
  
