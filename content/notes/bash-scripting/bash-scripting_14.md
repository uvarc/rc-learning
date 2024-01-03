---
title: Curly Braces Keeping a tight wrap on variable names
date: 2023-11-29-21:03:38Z
type: docs 
weight: 750
menu: 
    bash-scripting:
---


Create script name.sh

_#!/bin/bash_

firstname=Peter

lastname=Miller

fullname="$firstname $lastname"

_# simple variable operation_

echo Hello $fullname

_#Try to echo "Where are the Millers?"_

echo Where are the $lastname s?

echo Where are the $lastnames?

echo Where are the ${lastname}s?

Curly braces {} are used to separate a variable name from adjacent characters.

