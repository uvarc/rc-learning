---
title: Extensions and Shortcuts
date: 2023-11-29-21:03:38Z
type: docs 
weight: 3100
menu: 
    bash-scripting:
---


Most shells and languages support some shortcuts:

\w : [A-Za-z0-9_]

\s : [ \t\r\n]  some flavors have a few more rare whitespace characters

\d : [0-9]

\D : ^\d

^\W: ^\w

^\S: ^\s

NB \D\S is not the same as ^\d\s; in fact it matches anything. ^\d\s matches a but not 1

