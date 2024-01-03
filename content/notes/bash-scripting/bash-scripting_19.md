---
title: Example String Comparisons
date: 2023-11-29-21:03:38Z
type: docs 
weight: 1000
menu: 
    bash-scripting:
---


__Create new script __  __ifname.sh__

* _#!/bin/bash_
* if [[ $name == "Tom" ]]; then
  * echo ${name}, you are assigned to Room 12
* elif [[ $name == "Harry" ]]; then
* echo ${name}, please go to Room 3
* elif [[ -z $name ]]; then
* echo You did not tell me your name
* else
* echo "${name}, I don’t know where \
* you are supposed to go"
* fi

