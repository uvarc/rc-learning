---
title: More About Looping
toc: true
type: docs
draft: false
weight: 44

menu:
    python_introduction:
        parent: Conditionals and Loops
        weight: 44
---

## Leaving Early

To exit a loop prior to its normal termination, use the `break` statement.  

If `break` is executed, any `else` clause will _not_ be executed.

Break can exit only from the loop level in which it occurs.  In Python this is the indentation level.

To skip the rest of the loop instructions and go to the next cycle, use `continue`. Similarly to `break`, it skips only the rest of the statements at its own level.

```python
x=1.
while x>0:
    x+=1.
    if x>=1000.0: break 
    if x<100.0: continue
    x+=20.
    print(x)
```

Now we can better understand the purpose of the `else` clause.  One application could be to provide a warning for cases where a loop terminating normally may indicate a failure.  For example, if we were iterating on an algorithm that converges to an answer but may go astray, we can set a limit on the maximum number of iterations. In this example, optimize is a function that we invoke.  It may be in another package or it may be something we wrote.  The variable `f` stands for the function we are optimizing.  Therefore this code fragment is incomplete and cannot be run as is; it is an example of how `else` works.

```python
max_iter=1000000000
tol=1.e-14
iter=0
while iter<max_iter:
    x_new=optimize(f,x_old)
    iter+-1
    if abs(x_new-x_old)<tol:
        break
    x_old=x_new
else:
    print("Warning: max_iter exceeded, solution may not be valid.")
```

## Repeat/Until

A while loop is equivalent to

```python
while True:
    if not condition:
        break 
    codeblock
```

The while always tests at the _top_ of the loop.  If we wish to test elsewhere we can use break to accomplish this.  If we test at the bottom of the loop, the pattern is often called _repeat/until_.

```python
while True:
   codeblock
   if condition: break 
```

Example:

```python
x=1.
while True:
    z=x-1
    x+=1
    if x>10: break 
```

### Nested Loops

We can write loops within loops.  The outer loop variable remains fixed while the inner one goes through its iterator; then the outer one takes the next value and the entire inner loop is repeated.

```python
for i in range(5):
    for j in range(10):
        print(i,j)
```

#### Reinitializing

In nested loops, if we need to recompute something we often need to reinitialize a variable.  Examine the difference between

{{< code-snippet >}}
s=0.
for i in range(10):
    for j in range(15):
        s=s+i+j
print(s)
{{< /code-snippet >}}

and 

{{< code-snippet >}}
for i in range(10):
    s=0 
    for j in range(10):
        s=s+i+j
print(s)
{{< /code-snippet >}}
