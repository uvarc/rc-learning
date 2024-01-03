---
title: continue
date: 2023-11-29-21:03:38Z
type: docs 
weight: 1650
menu: 
    bash-scripting:
---


To skip the commands for an iteration use continue

* for i in iterator
* do
  * if [[ condition ]]
  * then
  * continue # skips to next iteration
  * fi
  * command_1
  * command_2
* done

