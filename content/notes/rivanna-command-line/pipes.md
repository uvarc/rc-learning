---
title: Pipes
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1350
menu: 
    rivanna-command-line:
---

One of the most powerful properties of Unix is that you can  __pipe__  the standard output of one command into the standard input of another.

The pipe symbol `|` is above the backslash on most US keyboards.  Pipes can be chained indefinitely, though most uses need just one.
```no-highlight
cmd1 | cmd2 | cmd3
```

**Example**

Commands such as `ls` have lengthy manpages that will tend to scroll off our terminal window.
```bash 
$man ls | more
```
Now we can page through the listing.
