---
title: “Three-Expression” For Loop
date: 2023-11-29-21:03:38Z
type: docs 
weight: 1500
menu: 
    bash-scripting:
---


for (( EXPR1; EXPR2; EXPR3 ))

do

statements

done

__Open __  __forloop.sh__  __ and add:__

name="file"

IMAX=10

for (( i=0 ; i<${IMAX} ; i=i+2 )); do

echo "${name}.${i}"

done

