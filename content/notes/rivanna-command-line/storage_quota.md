---
title: Checking your Storage
date: 2023-12-11T21:14:11-14:00
type: docs 
weight: 1750
menu: 
    rivanna-command-line:
---

To see how much disk space, you have used in your home and scratch directories, open a terminal window and type  `hdquota`  at the command-line prompt:

```bash
$hdquota
Type    Location         Name                  Size Used Avail Use%
=======================================================================
home       /home        mst3k                   51G   12G   39G  24%
Scratch    /project     slurmtests             2.0P  1.9P  144T  93%
Project    /project     arcs                    16T   12T  3.8T  75%   Project    /project     rivanna_software       1.1T  4.2M  1.0T   1%   Project    /project     ds5559                  51G  3.7G   47G   8%   Value      /nv          vol174                 5.5T  1.2T  4.4T  21%
```

Note: only home, and other storage just for some

