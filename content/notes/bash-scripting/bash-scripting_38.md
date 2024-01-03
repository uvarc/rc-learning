---
title: String Operations
date: 2023-11-29-21:03:38Z
type: docs 
weight: 1950
menu: 
    bash-scripting:
---


* Bash has a number of built-in string operations.
* Concatenation
  * Just write them together (literals should be quoted)
  * newstring=${string}".ext"
* String length
  * ${#string}
* Extract substring
  * Strings are zero-indexed, first character is numbered 0
  * ${string:pos}  _# Extract from _  _pos_  _ to the end_
  * ${string:pos:len}  _# Extract _  _len_  _ characters _
  * _		         starting at _  _pos_

