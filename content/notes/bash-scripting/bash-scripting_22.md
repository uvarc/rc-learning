---
title: Testing File Properties
date: 2023-11-29-21:03:38Z
type: docs 
weight: 1150
menu: 
    bash-scripting:
---


* This is a very common occurrence in bash scripts so we have a set of operators just for this.
* The most common operators are:
  * -e <file>   :  file exists
  * -f <file>   :  file exists and is a regular file
  * -s <file>   :  file exists and has length > 0
  * -d <dir>    : exists and is a directory

