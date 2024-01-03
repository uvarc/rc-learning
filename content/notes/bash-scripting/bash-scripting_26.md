---
title: Example
date: 2023-11-29-21:03:38Z
type: docs 
weight: 1350
menu: 
    bash-scripting:
---


case $filename in

*.c)

echo "C source file"

;;

*.py)

echo "Python script file"

;;

*)  _#optional, indicates default_

echo "I don’t know what this file is"

;;

esac

