---
title: Example Combine while and case
date: 2023-11-29-21:03:38Z
type: docs 
weight: 1700
menu: 
    bash-scripting:
---


* _while_  plus  _case_
* while [[ $# -gt 0 ]]; do
* case "$1" in
  * -v)  verbose="on";;
  * -*)  echo >&2 "USAGE: $0 [-v] [file]"
    * exit 1;;
  * *)   break;;   # default
* esac
* shift
* done

