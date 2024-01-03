---
title: sed examples
date: 2023-11-29-21:03:38Z
type: docs 
weight: 3250
menu: 
    bash-scripting:
---


* sed operates on standard input and outputs to stdout unless told otherwise.  Thus you must redirect.
* The -i option will tell it to overwrite the old file.  This is often a mistake (if you do this, be sure to debug the sed command carefully first).
* sed 'command' < old > new
  * Note hard quotes—best practice is to use them
* sed 's/day/night/' < old > new
  * Remember, expressions are  _greedy_ ; day/night here changes Sunday to Sunnight

