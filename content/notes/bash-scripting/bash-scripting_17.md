---
title: Conditionals
date: 2023-11-29-21:03:38Z
type: docs 
weight: 900
menu: 
    bash-scripting:
---


* The shell has an if else construct to allow execution of distinct command sets based on a predefined condition
  * if [[ <condition> ]]
  * then
  * commands
  * elif [[ <condition> ]]    _#optional         _      	commands
  * else                      _#optional_
  * more commands
  * fi  _#ends the if condition block _

__Note: A space is required after __  __[[ __  __and before  __  __]]__

