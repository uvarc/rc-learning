---
title: While Loops
toc: true
type: docs
draft: false
weight: 43

menu:
    python_introduction:
        parent: Conditionals and Loops
        weight: 43
---

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

**Exercise**

Modify each for loop in the previous [section's](/courses/python_introduction/for_loops) exercise to use a while loop instead.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/exercises/sum_to_N_while.py" lang="python" >}}
{{< /spoiler >}}

