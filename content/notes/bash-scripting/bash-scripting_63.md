---
title: grep examples
date: 2023-11-29-21:03:38Z
type: docs 
weight: 3200
menu: 
    bash-scripting:
---


* grep "regex" filename
  * The quotes are often needed; they make sure it’s interpreted as a regex
  * We assume Gnu grep on Linux (it is also called egrep)
  * egrep matches anywhere in the string
* grep ^root /etc/passwd
* grep :$ /etc/passwd

