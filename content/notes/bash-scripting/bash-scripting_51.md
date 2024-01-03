---
title: Function Variables
date: 2023-11-29-21:03:38Z
type: docs 
weight: 2600
menu: 
    bash-scripting:
---


Variables set in the function are global to the script!

#!/bin/bash

myvar="hello"

myfunc() {

myvar="one two three"

for x in $myvar; do

echo $x

done

}

myfunc  _# call function_

echo $myvar $x

