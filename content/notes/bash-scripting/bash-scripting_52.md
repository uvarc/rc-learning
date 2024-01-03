---
title: Making Local Variables
date: 2023-11-29-21:03:38Z
type: docs 
weight: 2650
menu: 
    bash-scripting:
---


We can use the keyword local to avoid clobbering our global variables.

#!bin/bash

myvar="hello"

myfunc() {

local x

local myvar="one two three"

for x in $myvar ; do

echo $x

done

}

myfunc echo $myvar $x

