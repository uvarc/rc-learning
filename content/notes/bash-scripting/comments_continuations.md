---
title: Comments and Line Continuations
date: 2023-11-29-21:03:38Z
type: docs 
weight: 450
menu: 
    bash-scripting:
---

Comments start with the hash mark `#`.  All text from the `#` to the end of the line is ignored.

The backslash `\` is the line-continuation symbol. The next line of text will be treated as an extension of the previous line.

```bash
#This is a comment
echo "This line is too long I \
want it to be on two lines"
```

Multiple statements can be placed on the same line when separated by the semicolon `;`.

```bash
a=20; b=3
```
As a general rule, if more than one statement is on a line they should be short.
