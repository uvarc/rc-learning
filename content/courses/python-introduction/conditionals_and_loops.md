---
title: Conditionals and Loops 
toc: false
type: docs
draft: false
weight: 40
date: "2020-11-17T00:00:00"

menu:
    python-introduction:
---

Now that we understand the basic data types, we can begin to study the fundamental programming constructs that all programs will use.  The two most important of these are _conditionals_ and _loops_.  Almost every script we write will require one or both of these.

## Code Blocks

Both loops and conditionals, as well as many other constructs we will encounter later, work with _code blocks_.  A code block is a group of statements that function logically as a single statement.  

Most of the time, a block is introduced by a colon (`:`) following a declaration or expression.  In Python these blocks are indicated by the _indentation level_. You may indent each block by however many spaces you wish (3 or 4 is usually recommended), but each block level must be indented by exactly the same number.  Do not use tabs. The block is terminated by returning to the previous indentation level; there are no special characters or statements to delineate a block.

Many editors, including Spyder, will automatically indent the next statement to the same level as the previous one.  You escape to an outer level with the backspace or some other key.  Spyder also provides _Indent_ and _Unindent_ options in its Edit menu.  These are extremely convenient for Python since you need only select lines, then click indent or unindent to create or move a code block level.

**Examples**

```python
if x==20:
    x=99
    if (x>=30):
        for i in range(x):
            j=i+x
print(j)

for i in range(1,101,10):
    x=float(i)**2
    print(i,x)
z=30.
```


