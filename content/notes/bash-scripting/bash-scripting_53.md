---
title: Return Values
date: 2023-11-29-21:03:38Z
type: docs 
weight: 2700
menu: 
    bash-scripting:
---


* Strictly speaking, a function returns  _only_  its exit status.
* The returned value must be an integer
* You can get the value with $?
* e.g.
  * myfunc $1 $2
  * result=$?

