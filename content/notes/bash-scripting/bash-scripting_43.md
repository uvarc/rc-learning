---
title: Array Example
date: 2023-11-29-21:03:38Z
type: docs 
weight: 2200
menu: 
    bash-scripting:
---


_!#/bin/bash_

arr=(blue red green)

for (( i=0 ; i<${#arr[@]} ; i++ ))

do

echo ${arr[i]}

done

_#!/bin/bash_

arr=(blue red green)

for color in ${arr[@]}

do

echo ${color}

done

