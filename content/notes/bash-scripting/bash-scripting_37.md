---
title: Example 2
date: 2023-11-29-21:03:38Z
type: docs 
weight: 1900
menu: 
    bash-scripting:
---


__Create script __  __commandline.sh__

* _#!/bin/bash _
* USAGE="Usage: $0 arg1 arg2 arg3 ...argN”
* if [[ "$#" -eq 0 ]]; then
* <span style="color:#0070C0"># no command line arguments</span>
  * echo "$USAGE"
  * exit 1  _# return to command line_
* fi
* echo "All arguments: $@"
* i=1  _# counter_
* while [[ "$#" -gt 0 ]]; do
  * echo "Argument ${i}: $1"
  * shift   _# move _  _args_  _ 1 position to the left_
  * ((i++))  _# increment counter_
* done

