---
title: Clipping Strings
date: 2023-11-29-21:03:38Z
type: docs 
weight: 2000
menu: 
    bash-scripting:
---


* It is very common in bash scripts to clip off part of a string so it can be remodeled.
  * Delete shortest match from front of string
  * ${string#substring}
  * Delete longest match from front of string
  * ${string##substring}
  * Delete shortest match from back of string
  * ${string%substring}
  * Delete longest match from back of string
  * ${string%%substring}

