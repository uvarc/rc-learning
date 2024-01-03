---
title: Executing a Command Within a Script
date: 2023-11-29-21:03:38Z
type: docs 
weight: 650
menu: 
    bash-scripting:
---

Characters between a pair of backticks  <span style="color:#FF0000">`</span>  are interpreted and executed as commands

The output of an executed command can be stored in variable or used as input for another command

In current shell versions $(COMMAND) is equivalent to `COMMAND`

__Create new script __  __command.sh__

_#!/bin/bash_

echo  <span style="color:#FF0000">`</span> date <span style="color:#FF0000">`</span>   _# exec date command, print output_

p= <span style="color:#FF0000"> __`__ </span> pwd <span style="color:#FF0000"> __`__ </span>   _# assign output to variable first_

echo Current directory: $p

