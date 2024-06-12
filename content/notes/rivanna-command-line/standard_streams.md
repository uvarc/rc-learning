---
title: Standard Streams
date: 2023-12-11T21:14:11-14:00
type: docs 
weight: 1250
menu: 
    rivanna-command-line:
---

Each executable has associated with it three input/output streams:  __standard input__ ,  __standard error__ , and  __standard output__.  Normally these streams come from or go to your console (i.e. your terminal).

Most Unix commands read from standard input and/or write to standard output.

These I/O streams are often represented as  __stdin__,  __stderr__, and  __stdout__.

The Unix commands we have studied so far all write to standard output.

```bash
ls -l
```
produces output to the terminal.

