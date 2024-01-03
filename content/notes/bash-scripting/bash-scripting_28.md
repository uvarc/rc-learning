---
title: Examples
date: 2023-11-29-21:03:38Z
type: docs 
weight: 1450
menu: 
    bash-scripting:
---


__Create new script __  __forloop.sh__  __:__

* for i in 1 2 3 4 5 ; do
  * echo "Loop 1: I am in step $i"
* done
* for i in {1..5} ; do  #bash 3.0 and up
  * echo "Loop 2: I am in step $i"
* done
* for i in 0{1..9} {90..100} ; do
* echo "Loop 3: I am in step $i"
* done

