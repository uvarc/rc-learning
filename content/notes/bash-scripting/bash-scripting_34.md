---
title: Bash Arithmetic
date: 2023-11-29-21:03:38Z
type: docs 
weight: 1750
menu: 
    bash-scripting:
---


* We said earlier that bash is bad at arithmetic, but some basic operations can be performed.
* Expressions to be evaluated numerically must be enclosed in  _double parentheses_ .
  * x=$((4+20))
* i=$(($x+1))

It works only with  _integers_ . If you need more advanced math (even just fractions!) you must use bc.

