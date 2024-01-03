---
title: Function Arguments
date: 2023-11-29-21:03:38Z
type: docs 
weight: 2550
menu: 
    bash-scripting:
---


* Function arguments are passed in the  _caller_ .  In the function they are treated like command-line options.
  * #!/bin/bash
  * function writeout() {
  * echo $1
  * }
  * writeout "Hello World"

