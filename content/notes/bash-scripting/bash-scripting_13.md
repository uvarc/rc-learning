---
title: Escaping Special Characters
date: 2023-11-29-21:03:38Z
type: docs 
weight: 700
menu: 
    bash-scripting:
---


Bash uses several special characters like $, “, ‘, `, , and others that are interpreted with special meaning.

At your bash prompt, type

echo Cool. You won $100,000 in the lottery.

_# Try with escaping the $ character_

Echo Cool. You won  <span style="color:#FF0000"></span> $100,000 in the lottery.

The  <span style="color:#FF0000"></span>  character instructs the interpreter to treat the next character as a literal and ignore (escape) any possible special meaning

